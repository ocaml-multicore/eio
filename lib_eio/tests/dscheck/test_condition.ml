let debug = false

exception Abort

module T = Condition

(* [prod] threads increment a counter and notify a condition.
   A consumer watches the condition and waits until it has seen
   all of them. We check that the client always sees the final value.
   If [cancel] is set, we also try to cancel the client and accept
   that as success too. *)
let test ~prod ~cancel () =
  let t = T.create () in
  let sent = Atomic.make 0 in
  for _ = 1 to prod do
    Atomic.spawn (fun () ->
        Atomic.incr sent;
        T.broadcast t
      )
  done;
  let finished = ref false in
  Atomic.spawn (fun () ->
      let ctx =
        Fake_sched.run @@ fun () ->
        try
          T.loop_no_mutex t (fun () ->
              if Atomic.get sent = prod && not cancel then Some ()
              else None
            );
          finished := true
        with T.Cancel.Cancelled Abort ->
          finished := true
       in
       if cancel then
         Option.iter (fun c -> T.Cancel.cancel c Abort) ctx
    );
  Atomic.final (fun () ->
      Atomic.check (fun () -> !finished);
      if debug then (
        Fmt.pr "%a@." Broadcast.dump t;
      );
    )

let () =
  Atomic.trace (test ~prod:2 ~cancel:false);
  Atomic.trace (test ~prod:2 ~cancel:true)
