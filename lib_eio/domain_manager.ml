class virtual t = object
  method virtual run : 'a. (cancelled:exn Promise.t -> 'a) -> 'a
  method virtual run_raw : 'a. (unit -> 'a) -> 'a
end

let run_raw (t : #t) = t#run_raw

let run (t : #t) fn =
  t#run @@ fun ~cancelled ->
  (* If the spawning fiber is cancelled, [cancelled] gets set to the exception. *)
  try
    Fiber.first
      (fun () ->
         match Promise.await cancelled with
         | Cancel.Cancelled ex -> raise ex    (* To avoid [Cancelled (Cancelled ex))] *)
         | ex -> raise ex (* Shouldn't happen *)
      )
      fn
  with ex ->
    match Promise.peek cancelled with
    | Some (Cancel.Cancelled ex2 as cex) when ex == ex2 ->
      (* We unwrapped the exception above to avoid [fn] seeing a double cancelled exception.
         But this means that the top-level reported the original exception,
         which isn't what we want. *)
      raise cex
    | _ -> raise ex
