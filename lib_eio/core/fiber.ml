type _ Effect.t += Fork : Cancel.fiber_context * (unit -> unit) -> unit Effect.t

let yield () =
  let fiber = Suspend.enter (fun fiber enqueue -> enqueue (Ok fiber)) in
  Cancel.check fiber.cancel_context

(* Note: [f] must not raise an exception, as that would terminate the whole scheduler. *)
let fork_raw new_fiber f =
  Effect.perform (Fork (new_fiber, f))

let fork ~sw f =
  Switch.check_our_domain sw;
  if Cancel.is_on sw.cancel then (
    let new_fiber = Cancel.Fiber_context.make ~cc:sw.cancel in
    fork_raw new_fiber @@ fun () ->
    Switch.with_op sw @@ fun () ->
    match f () with
    | () ->
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:None
    | exception ex ->
      Switch.fail sw ex;  (* The [with_op] ensures this will succeed *)
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:(Some ex)
  ) (* else the fiber should report the error to [sw], but [sw] is failed anyway *)

let fork_daemon ~sw f =
  Switch.check_our_domain sw;
  if Cancel.is_on sw.cancel then (
    let new_fiber = Cancel.Fiber_context.make ~cc:sw.cancel in
    fork_raw new_fiber @@ fun () ->
    Switch.with_daemon sw @@ fun () ->
    match f () with
    | `Stop_daemon ->
      (* The daemon asked to stop. *)
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:None
    | exception Cancel.Cancelled Exit when not (Cancel.is_on sw.cancel) ->
      (* The daemon was cancelled because all non-daemon fibers are finished. *)
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:None
    | exception ex ->
      Switch.fail sw ex;  (* The [with_daemon] ensures this will succeed *)
      Ctf.note_resolved (Cancel.Fiber_context.tid new_fiber) ~ex:(Some ex)
  ) (* else the fiber should report the error to [sw], but [sw] is failed anyway *)

let fork_promise ~sw f =
  Switch.check_our_domain sw;
  let new_fiber = Cancel.Fiber_context.make ~cc:sw.Switch.cancel in
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
  let new_fiber = Cancel.Fiber_context.make ~cc:sw.Switch.cancel in
  let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
  fork_raw new_fiber (fun () ->
      match Switch.with_op sw f with
      | x -> Promise.resolve r x
      | exception ex ->
        Switch.fail sw ex  (* The [with_op] ensures this will succeed *)
    );
  p

let all xs =
  Switch.run @@ fun sw ->
  List.iter (fork ~sw) xs

let both f g = all [f; g]

let pair f g =
  Switch.run @@ fun sw ->
  let x = fork_promise ~sw f in
  let y = g () in
  (Promise.await_exn x, y)

let fork_sub ~sw ~on_error f =
  fork ~sw (fun () ->
      try Switch.run f
      with
      | ex when Cancel.is_on sw.cancel ->
        (* Typically the caller's context is within [sw], but it doesn't have to be.
           It's possible that the original context has finished by now,
           but [fork] is keeping [sw] alive so we can use that report the error. *)
        Switch.run_in sw @@ fun () ->
        try on_error ex
        with ex2 ->
          (* The [run_in] ensures [adopting_sw] isn't finished here *)
          Switch.fail sw ex;
          Switch.fail sw ex2
    )

exception Not_first

let await_cancel () =
  Suspend.enter @@ fun fiber enqueue ->
  Cancel.Fiber_context.set_cancel_fn fiber (fun ex -> enqueue (Error ex))

let any fs =
  let r = ref `None in
  let parent_c =
    Cancel.sub_unchecked (fun cc ->
        let wrap h =
          match h () with
          | x ->
            begin match !r with
              | `None -> r := `Ok x; Cancel.cancel cc Not_first
              | `Ex _ | `Ok _ -> ()
            end
          | exception Cancel.Cancelled _ when not (Cancel.is_on cc) ->
            (* If this is in response to us asking the fiber to cancel then we can just ignore it.
               If it's in response to our parent context being cancelled (which also cancels [cc]) then
               we'll check that context and raise it at the end anyway. *)
            ()
          | exception ex ->
            begin match !r with
              | `None -> r := `Ex (ex, Printexc.get_raw_backtrace ()); Cancel.cancel cc ex
              | `Ok _ -> r := `Ex (ex, Printexc.get_raw_backtrace ())
              | `Ex prev ->
                let bt = Printexc.get_raw_backtrace () in
                r := `Ex (Exn.combine prev (ex, bt))
            end
        in
        let rec aux = function
          | [] -> await_cancel ()
          | [f] -> wrap f; []
          | f :: fs ->
            let new_fiber = Cancel.Fiber_context.make ~cc in
            let p, r = Promise.create_with_id (Cancel.Fiber_context.tid new_fiber) in
            fork_raw new_fiber (fun () ->
                match wrap f with
                | x -> Promise.resolve_ok r x
                | exception ex -> Promise.resolve_error r ex
              );
            p :: aux fs
        in
        let ps = aux fs in
        Cancel.protect (fun () -> List.iter Promise.await_exn ps)
      )
  in
  match !r, Cancel.get_error parent_c with
  | `Ok r, None -> r
  | (`Ok _ | `None), Some ex -> raise ex
  | `Ex (ex, bt), None -> Printexc.raise_with_backtrace ex bt
  | `Ex ex1, Some ex2 ->
    let bt2 = Printexc.get_raw_backtrace () in
    let ex, bt = Exn.combine ex1 (ex2, bt2) in
    Printexc.raise_with_backtrace ex bt
  | `None, None -> assert false

let first f g = any [f; g]

let check () =
  let ctx = Effect.perform Cancel.Get_context in
  Cancel.check ctx.cancel_context

(* Some concurrent list operations *)

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
    if t.free_fibers = 0 then Single_waiter.await t.cond t.sw.id;
    (* If we got woken up then there was a free fiber then. And since we're the
       only fiber that uses [t], and we were sleeping, it must still be free. *)
    assert (t.free_fibers > 0);
    t.free_fibers <- t.free_fibers - 1

  let release t =
    t.free_fibers <- t.free_fibers + 1;
    if t.free_fibers = 1 then Single_waiter.wake t.cond (Ok ())

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
    Switch.run @@ fun sw ->
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
    Switch.run @@ fun sw ->
    let limiter = Limiter.create ~sw max_fibers in
    let rec aux = function
      | [] -> ()
      | [x] -> Limiter.use limiter fn x
      | x :: xs ->
        Limiter.fork limiter fn x;
        aux xs
    in
    aux items
