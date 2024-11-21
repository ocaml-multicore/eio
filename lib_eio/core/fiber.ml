[@@@alert "-unstable"]

type _ Effect.t += Fork : Cancel.fiber_context * (unit -> unit) -> unit Effect.t

let yield () =
  let fiber = Suspend.enter "" (fun fiber enqueue -> enqueue (Ok fiber)) in
  Cancel.check fiber.cancel_context

(* Note: [f] must not raise an exception, as that would terminate the whole scheduler. *)
let fork_raw new_fiber f =
  Effect.perform (Fork (new_fiber, f))

let fork ~sw f =
  Switch.check_our_domain sw;
  if Cancel.is_on sw.cancel then (
    let vars = Cancel.Fiber_context.get_vars () in
    let new_fiber = Cancel.Fiber_context.make ~cc:sw.cancel ~vars in
    fork_raw new_fiber @@ fun () ->
    Switch.with_op sw @@ fun () ->
    try
      f ()
    with ex ->
      let bt = Printexc.get_raw_backtrace () in
      Switch.fail ~bt sw ex;  (* The [with_op] ensures this will succeed *)
  ) (* else the fiber should report the error to [sw], but [sw] is failed anyway *)

let fork_daemon ~sw f =
  Switch.check_our_domain sw;
  if Cancel.is_on sw.cancel then (
    let vars = Cancel.Fiber_context.get_vars () in
    let new_fiber = Cancel.Fiber_context.make ~cc:sw.cancel ~vars in
    fork_raw new_fiber @@ fun () ->
    Switch.with_daemon sw @@ fun () ->
    match f () with
    | `Stop_daemon ->
      (* The daemon asked to stop. *)
      ()
    | exception Cancel.Cancelled Exit when not (Cancel.is_on sw.cancel) ->
      (* The daemon was cancelled because all non-daemon fibers are finished. *)
      ()
    | exception ex ->
      Switch.fail sw ex;  (* The [with_daemon] ensures this will succeed *)
  ) (* else the fiber should report the error to [sw], but [sw] is failed anyway *)

let fork_promise ~sw f =
  Switch.check_our_domain sw;
  let vars = Cancel.Fiber_context.get_vars () in
  let new_fiber = Cancel.Fiber_context.make ~cc:sw.Switch.cancel ~vars in
  let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
  fork_raw new_fiber (fun () ->
      match Switch.with_op sw f with
      | x -> Promise.resolve_ok r x
      | exception ex -> Promise.resolve_error r ex        (* Can't fail; only we have [r] *)
    );
  p

(* This is not exposed. On failure it fails [sw], but you need to make sure that
   any fibers waiting on the promise will be cancelled. *)
let fork_promise_exn ~sw f =
  Switch.check_our_domain sw;
  let vars = Cancel.Fiber_context.get_vars () in
  let new_fiber = Cancel.Fiber_context.make ~cc:sw.Switch.cancel ~vars in
  let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
  fork_raw new_fiber (fun () ->
      match Switch.with_op sw f with
      | x -> Promise.resolve r x
      | exception ex ->
        Switch.fail sw ex  (* The [with_op] ensures this will succeed *)
    );
  p

(* Like [List.iter (fork ~sw)], but runs the last one in the current fiber
   for efficiency and less cluttered traces. *)
let rec forks ~sw = function
  | [] -> ()
  | [x] -> Switch.check sw; x ()
  | x :: xs ->
    fork ~sw x;
    forks ~sw xs

let all xs =
  Switch.run ~name:"all" @@ fun sw ->
  forks ~sw xs

let both f g =
  Switch.run ~name:"both" @@ fun sw ->
  forks ~sw [f; g]

let pair f g =
  Switch.run ~name:"pair" @@ fun sw ->
  let x = fork_promise ~sw f in
  let y = g () in
  (Promise.await_exn x, y)

exception Not_first

let await_cancel () =
  Suspend.enter "await_cancel" @@ fun fiber enqueue ->
  Cancel.Fiber_context.set_cancel_fn fiber (fun ex -> enqueue (Error ex))

type 'a any_status =
  | New
  | Ex of (exn * Printexc.raw_backtrace)
  | OK of 'a

let any_gen ~return ~combine fs =
  let r = ref New in
  let parent_c =
    Cancel.sub_unchecked Any (fun cc ->
        let wrap h =
          match h () with
          | x ->
            begin match !r with
              | New -> r := OK (return x); Cancel.cancel cc Not_first
              | OK prev -> r := OK (combine prev x)
              | Ex _ -> ()
            end
          | exception Cancel.Cancelled _ when not (Cancel.is_on cc) ->
            (* If this is in response to us asking the fiber to cancel then we can just ignore it.
               If it's in response to our parent context being cancelled (which also cancels [cc]) then
               we'll check that context and raise it at the end anyway. *)
            ()
          | exception ex ->
            begin match !r with
              | New -> r := Ex (ex, Printexc.get_raw_backtrace ()); Cancel.cancel cc ex
              | OK _ -> r := Ex (ex, Printexc.get_raw_backtrace ())
              | Ex prev ->
                let bt = Printexc.get_raw_backtrace () in
                r := Ex (Exn.combine prev (ex, bt))
            end
        in
        let vars = Cancel.Fiber_context.get_vars () in
        let rec aux = function
          | [] -> await_cancel ()
          | [f] -> wrap f; []
          | f :: fs ->
            let new_fiber = Cancel.Fiber_context.make ~cc ~vars in
            let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
            fork_raw new_fiber (fun () ->
                match wrap f with
                | () -> Promise.resolve_ok r ()
                | exception ex -> Promise.resolve_error r ex
              );
            p :: aux fs
        in
        let ps = aux fs in
        Cancel.protect (fun () -> List.iter Promise.await_exn ps)
      )
  in
  match !r, Cancel.get_error parent_c with
  | OK r, None -> r
  | (OK _ | New), Some ex -> raise ex
  | Ex (ex, bt), None -> Printexc.raise_with_backtrace ex bt
  | Ex ex1, Some ex2 ->
    let bt2 = Printexc.get_raw_backtrace () in
    let ex, bt = Exn.combine ex1 (ex2, bt2) in
    Printexc.raise_with_backtrace ex bt
  | New, None -> assert false

let n_any fs =
  List.rev (any_gen fs ~return:(fun x -> [x]) ~combine:(fun xs x -> x :: xs))

let any ?(combine=(fun x _ -> x)) fs = any_gen fs ~return:Fun.id ~combine

let first ?combine f g = any ?combine [f; g]

let is_cancelled () =
  let ctx = Effect.perform Cancel.Get_context in
  not (Cancel.is_on ctx.cancel_context)

let check () =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.check ctx.cancel_context

(* Some concurrent list operations *)
module List = struct

  let opt_cons x xs =
    match x with
    | None -> xs
    | Some x -> x :: xs

  module Limiter : sig
    (** This is a bit like using a semaphore, but it assumes that there is only a
        single fiber using it. e.g. you must not call {!use}, {!fork}, etc from
        two different fibers. *)

    type t

    val create : sw:Switch.t -> int -> t
    (** [create ~sw n] is a limiter that allows running up to [n] jobs at once. *)

    val use : t -> ('a -> 'b) -> 'a -> 'b
    (** [use t fn x] runs [fn x] in this fiber, counting it as one use of [t]. *)

    val fork : t -> ('a -> unit) -> 'a -> unit
    (** [fork t fn x] runs [fn x] in a new fibre, once a fiber is free. *)

    val fork_promise_exn : t -> ('a -> 'b) -> 'a -> 'b Promise.t
    (** [fork_promise_exn t fn x] runs [fn x] in a new fibre, once a fiber is free,
        and returns a promise for the result. *)
  end = struct
    type t = {
      mutable free_fibers : int;
      cond : unit Single_waiter.t;
      sw : Switch.t;
    }

    let max_fibers_err n =
      Fmt.failwith "max_fibers must be positive (got %d)" n

    let create ~sw max_fibers =
      if max_fibers <= 0 then max_fibers_err max_fibers;
      {
        free_fibers = max_fibers;
        cond = Single_waiter.create ();
        sw;
      }

    let await_free t =
      if t.free_fibers = 0 then Single_waiter.await t.cond "Limiter.await_free" t.sw.cancel.id;
      (* If we got woken up then there was a free fiber then. And since we're the
         only fiber that uses [t], and we were sleeping, it must still be free. *)
      assert (t.free_fibers > 0);
      t.free_fibers <- t.free_fibers - 1

    let release t =
      t.free_fibers <- t.free_fibers + 1;
      if t.free_fibers = 1 then Single_waiter.wake_if_sleeping t.cond

    let use t fn x =
      await_free t;
      let r = fn x in
      release t;
      r

    let fork_promise_exn t fn x =
      await_free t;
      fork_promise_exn ~sw:t.sw (fun () -> let r = fn x in release t; r)

    let fork t fn x =
      await_free t;
      fork ~sw:t.sw (fun () -> fn x; release t)
  end

  let filter_map ?(max_fibers=max_int) fn items =
    match items with
    | [] -> []    (* Avoid creating a switch in the simple case *)
    | items ->
      Switch.run ~name:"filter_map" @@ fun sw ->
      let limiter = Limiter.create ~sw max_fibers in
      let rec aux = function
        | [] -> []
        | [x] -> Option.to_list (Limiter.use limiter fn x)
        | x :: xs ->
          let x = Limiter.fork_promise_exn limiter fn x in
          let xs = aux xs in
          opt_cons (Promise.await x) xs
      in
      aux items

  let map ?max_fibers fn = filter_map ?max_fibers (fun x -> Some (fn x))
  let filter ?max_fibers fn = filter_map ?max_fibers (fun x -> if fn x then Some x else None)

  let iter ?(max_fibers=max_int) fn items =
    match items with
    | [] -> ()    (* Avoid creating a switch in the simple case *)
    | items ->
      Switch.run ~name:"iter" @@ fun sw ->
      let limiter = Limiter.create ~sw max_fibers in
      let rec aux = function
        | [] -> ()
        | [x] -> Limiter.use limiter fn x
        | x :: xs ->
          Limiter.fork limiter fn x;
          aux xs
      in
      aux items

end

type 'a key = 'a Hmap.key

let create_key () = Hmap.Key.create ()

let get key = Hmap.find key (Cancel.Fiber_context.get_vars ())

let with_binding var value fn =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.Fiber_context.with_vars ctx (Hmap.add var value ctx.vars) fn

let without_binding var fn =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.Fiber_context.with_vars ctx (Hmap.rem var ctx.vars) fn

(* Coroutines.

   [fork_coroutine ~sw fn] creates a new fiber for [fn]. [fn] immediately suspends, setting its state to
   [Ready enqueue]. A consumer can resume it by setting the state to [Running] and calling [enqueue],
   while suspending itself. The consumer passes in its own [enqueue] function. They run alternatively
   like this, switching between the [Ready] and [Running] states.

   To finish, the coroutine fiber can set the state to [Finished] or [Failed],
   or the client can set the state to [Client_cancelled].
*)

(* Note: we could easily generalise this to [('in, 'out) coroutine] if that was useful. *)
type 'out coroutine =
  [ `Init
  | `Ready of [`Running of 'out Suspend.enqueue] Suspend.enqueue
  | `Running of 'out Suspend.enqueue
  | `Finished
  | `Client_cancelled of exn
  | `Failed of exn ]

(* The only good reason for the state to change while the coroutine is running is if the client
   cancels. Return the exception in that case. If the coroutine is buggy it might e.g. fork two
   fibers and yield twice for a single request - return Invalid_argument in that case. *)
let unwrap_cancelled state =
  match Atomic.get state with
  | `Client_cancelled ex -> ex
  | `Finished | `Failed _ -> Invalid_argument "Coroutine has already stopped!"
  | `Ready _ -> Invalid_argument "Coroutine has already yielded!"
  | `Init | `Running _ -> Invalid_argument "Coroutine in unexpected state!"

let run_coroutine ~state fn =
  let await_request ~prev ~on_suspend =
    (* Suspend and wait for the consumer to resume us: *)
    Suspend.enter "await-consumer" (fun ctx enqueue ->
        let ready = `Ready enqueue in
        if Atomic.compare_and_set state prev ready then (
          Cancel.Fiber_context.set_cancel_fn ctx (fun ex ->
              if Atomic.compare_and_set state ready (`Failed ex) then
                enqueue (Error ex);
              (* else the client enqueued a resume for us; handle that instead *)
            );
          on_suspend ()
        ) else (
          enqueue (Error (unwrap_cancelled state))
        )
      )
  in
  let current_state = ref (await_request ~prev:`Init ~on_suspend:ignore) in
  fn (fun v ->
      (* The coroutine wants to yield the value [v] and suspend. *)
      let `Running enqueue as prev = !current_state in
      current_state := await_request ~prev ~on_suspend:(fun () -> enqueue (Ok (Some v)))
    );
  (* [fn] has finished. End the stream. *)
  if Atomic.compare_and_set state (!current_state :> _ coroutine) `Finished then (
    let `Running enqueue = !current_state in
    enqueue (Ok None)
  ) else (
    raise (unwrap_cancelled state)
  )

let fork_coroutine ~sw fn =
  let state = Atomic.make `Init in
  fork_daemon ~sw (fun () ->
      try
        run_coroutine ~state fn;
        `Stop_daemon
      with ex ->
        match ex, Atomic.exchange state (`Failed ex) with
          | _, `Running enqueue ->
            (* A client is waiting for us. Send the error there. Also do this if we were cancelled. *)
            enqueue (Error ex);
            `Stop_daemon
          | Cancel.Cancelled _, _ ->
            (* The client isn't waiting (probably it got cancelled, then we tried to yield to it and got cancelled too).
               If it tries to resume us later it will see the error. *)
            `Stop_daemon
          | _ ->
            (* Something unexpected happened. Re-raise. *)
            raise ex
    );
  fun () ->
    Suspend.enter "await-producer" (fun ctx enqueue ->
        let rec aux () =
          match Atomic.get state with
          | `Ready resume as prev ->
            let running = `Running enqueue in
            if Atomic.compare_and_set state prev running then (
              resume (Ok running);
              Cancel.Fiber_context.set_cancel_fn ctx (fun ex ->
                  if Atomic.compare_and_set state running (`Client_cancelled ex) then
                    enqueue (Error ex)
                )
            ) else aux ()
          | `Finished -> enqueue (Error (Invalid_argument "Coroutine has already finished!"))
          | `Failed ex | `Client_cancelled ex -> enqueue (Error (Invalid_argument ("Coroutine has already failed: " ^ Printexc.to_string ex)))
          | `Running _ -> enqueue (Error (Invalid_argument "Coroutine is still running!"))
          | `Init -> assert false
        in
        aux ()
      )

let fork_seq ~sw fn =
  Seq.of_dispenser (fork_coroutine ~sw fn)
