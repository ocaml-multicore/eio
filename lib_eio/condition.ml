(* Import these directly because we copy this file for the dscheck tests. *)
module Fiber_context = Eio__core.Private.Fiber_context
module Suspend = Eio__core.Private.Suspend
module Cancel = Eio__core.Cancel

type t = Broadcast.t

let create () = Broadcast.create ()

let lock_protected m =
  Cancel.protect (fun () -> Eio_mutex.lock m)

let await_generic ?mutex t =
  match
    Suspend.enter_unchecked (fun ctx enqueue ->
        match Fiber_context.get_error ctx with
        | Some ex ->
          Option.iter Eio_mutex.unlock mutex;
          enqueue (Error ex)
        | None ->
          match Broadcast.suspend t (fun () -> enqueue (Ok ())) with
          | None ->
            Option.iter Eio_mutex.unlock mutex
          | Some request ->
            Option.iter Eio_mutex.unlock mutex;
            Fiber_context.set_cancel_fn ctx (fun ex ->
                if Broadcast.cancel request then enqueue (Error ex)
                (* else already succeeded *)
              )
      )
  with
  | () -> Option.iter lock_protected mutex
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Option.iter lock_protected mutex;
    Printexc.raise_with_backtrace ex bt

let await t mutex = await_generic ~mutex t
let await_no_mutex t = await_generic t

let broadcast = Broadcast.resume_all

type request = Broadcast.request option

let register_immediate = Broadcast.suspend

let cancel = function
  | Some request -> Broadcast.cancel request
  | None -> false

let ensure_cancelled x = ignore (cancel x : bool)

type state =
  | Init
  | Waiting of ((unit, exn) result -> unit)
  | Done

(* There main property want is that we don't suspend forever if a broadcast
   happened after [fn] started, or if the fiber is cancelled.

   1. We start in the Init state.
   2. If a broadcast happens here we move to Done. If we later try to suspend, we'll resume immediately.
   3. We run [fn]. If a broadcast happens during this we'll transition to Done as before.
   4. If [fn] raises or wants to stop normally, we return without suspending at all.
   5. Otherwise, we suspend the fiber.
   6. We try to transition from Init to Waiting.
      If a broadcast transitioned to Done before this, we resume immediately.
      If a broadcast transitions afterwards, [wake] will see the [enqueue] function and wake us.
      Therefore, we can only sleep forever if a broadcast never happens after starting [fn].
   7. If the fiber is cancelled before suspending, we raise on suspend.
      If cancelled after suspending and before the request succeeds, we cancel the request and raise.
      If cancelled after the request succeeds, [wake] will resume us.
*)
let rec loop_no_mutex t fn =
  let state = Atomic.make Init in
  let wake () =
    match Atomic.exchange state Done with
    | Init -> ()        (* Broadcast happened before we suspended; suspend will notice *)
    | Waiting enqueue -> enqueue (Ok ())
    | Done -> assert false
  in
  let request = Broadcast.suspend t wake in
  (* Note: to avoid memory leaks, make sure that [request] is finished in all cases. *)
  match fn () with
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    ensure_cancelled request;
    Printexc.raise_with_backtrace ex bt
  | Some x ->
    ensure_cancelled request;
    x
  | None ->
    Suspend.enter_unchecked (fun ctx enqueue ->
        match Fiber_context.get_error ctx with
        | Some ex ->
          ensure_cancelled request;
          (* If a broadcast already happened, we still cancel. *)
          enqueue (Error ex)
        | None ->
          let waiting = Waiting enqueue in
          if Atomic.compare_and_set state Init waiting then (
            (* We were in Init, so [wake] hasn't yet done anything.
               When it runs, it will resume us.
               We're also not currently cancelled, because we checked above
               and cancellations only come from the same thread. *)
            Fiber_context.set_cancel_fn ctx (fun ex ->
                if cancel request then (
                  (* We could set the state to Done here, but there's no need;
                     we're not racing with anything now. [wake] never runs. *)
                  enqueue (Error ex)
                ) (* else we already got resumed *)
              )
          ) else (
            (* State is already Done, but [wake] couldn't wake us then
               because we hadn't moved to [waiting]. Resume now. *)
            enqueue (Ok ())
          )
      );
    loop_no_mutex t fn
