let debug = false

module T = Rcfd

let test ~n_users ~n_closers () =
  let messages = ref [] in
  let log fmt = (fmt ^^ "@.") |> Format.kasprintf @@ fun msg -> messages := msg :: !messages in
  if debug then log "== start ==";
  let wrapped_fd = Unix.make () in
  let t = T.make wrapped_fd in
  let n_closed = ref 0 in
  for _ = 1 to n_users do
    Atomic.spawn (fun () ->
        T.use t ~if_closed:ignore (fun fd ->
            log "Using FD";
            assert (Atomic.get fd = `Open);
            log "Releasing FD";
          )
      )
  done;
  for _ = 1 to n_closers do
    Atomic.spawn (fun () ->
        log "Closing FD";
        if T.close t then (
          log "Closed FD";
          incr n_closed
        ) else (
          log "FD already closed";
        )
      )
  done;
  Atomic.final (fun () ->
      if debug then List.iter print_string (List.rev !messages);
      assert (!n_closed = 1);
      assert (Atomic.get wrapped_fd = `Closed);
    )

let () =
  Atomic.trace (test ~n_users:2 ~n_closers:2);
