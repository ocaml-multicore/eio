module T = Pool
module Cancel = Eio__core.Cancel

exception Abort

(* [clients] threads try to use a pool of size [n].
   If [cancel] is set, they also try to cancel, and accept
   that as success too. *)
let test ~n ~clients ~cancel () =
  let t = T.create n (fun () -> ()) in
  let used = Atomic.make 0 in
  let finished = ref 0 in
  for _ = 1 to clients do
    Atomic.spawn (fun () ->
        let ctx =
          Fake_sched.run @@ fun () ->
          try
            T.use t (fun () -> Atomic.incr used);
            incr finished;
          with Cancel.Cancelled Abort ->
            incr finished;
        in
        if cancel then
          Option.iter (fun c -> Cancel.cancel c Abort) ctx
      )
  done;
  Atomic.final (fun () ->
      if not cancel then Atomic.check (fun () -> Atomic.get used = clients);
      Atomic.check (fun () -> !finished = clients);
    )

let () =
  Atomic.trace (test ~n:1 ~clients:2 ~cancel:false);
  Atomic.trace (test ~n:1 ~clients:2 ~cancel:true)
