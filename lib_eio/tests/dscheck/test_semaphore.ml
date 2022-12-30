let debug = false

module T = Sem_state

let test ~capacity ~users () =
  let messages = ref [] in
  let log fmt = (fmt ^^ "@.") |> Format.kasprintf @@ fun msg -> messages := msg :: !messages in
  if debug then log "== start ==";
  let t = T.create capacity in
  let running = Atomic.make 0 in
  let acquire fn =
    if T.acquire t then (fn (); None)
    else T.suspend t fn
  in
  for i = 1 to users do
    Atomic.spawn (fun () ->
        match
          acquire (fun () ->
              if debug then log "%d: got resource" i;
              Atomic.incr running;
              Atomic.decr running;
              if debug then log "%d: released resource" i;
              T.release t
            )
        with
        | None -> ()
        | Some request ->
          if T.cancel request then (
            if debug then log "%d: cancelled request" i;
          )
      )
  done;
  Atomic.every (fun () -> assert (Atomic.get running <= capacity));
  Atomic.final (fun () ->
      if debug then (
        List.iter print_string (List.rev !messages);
        Fmt.pr "%a@." T.dump t;
      );
      assert (Atomic.get t.state = capacity);
      (* Do a dummy non-cancelled operation to ensure the pointers end up together: *)
      T.resume t;
      assert (T.suspend t ignore = None);
      assert (T.Cells.Position.index t.cells.suspend =
              T.Cells.Position.index t.cells.resume);
    )

let () =
  Atomic.trace (test ~capacity:1 ~users:3);
  Atomic.trace (test ~capacity:2 ~users:3)
