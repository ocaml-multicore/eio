(* A simplified version of [Waiters] that can only handle one waiter and is not thread-safe. *)

type 'a t = {
  mutable wake : ('a, exn) result -> unit;
}

let create () = { wake = ignore }

let wake t v = t.wake v

let await t id =
  Suspend.enter @@ fun ctx enqueue ->
  Cancel.Fiber_context.set_cancel_fn ctx (fun ex ->
      t.wake <- ignore;
      enqueue (Error ex)
    );
  t.wake <- (fun x ->
      Cancel.Fiber_context.clear_cancel_fn ctx;
      t.wake <- ignore;
      Ctf.note_read ~reader:id ctx.tid;
      enqueue x
    )
