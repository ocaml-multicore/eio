open Effect

class virtual t = object
  method virtual run : 'a. (unit -> 'a) -> 'a
  method virtual run_raw : 'a. (unit -> 'a) -> 'a
end

let run_raw (t : #t) = t#run_raw

let run (t : #t) fn =
  let ctx = perform Cancel.Get_context in
  Cancel.check ctx.cancel_context;
  let cancelled, set_cancelled = Promise.create () in
  Cancel.Fibre_context.set_cancel_fn ctx (Promise.fulfill set_cancelled);
  (* If the spawning fibre is cancelled, [cancelled] gets set to the exception. *)
  match
    t#run @@ fun () ->
    Fibre.first
      (fun () ->
         match Promise.await cancelled with
         | Cancel.Cancelled ex -> raise ex    (* To avoid [Cancelled (Cancelled ex))] *)
         | ex -> raise ex (* Shouldn't happen *)
      )
      fn
  with
  | x ->
    ignore (Cancel.Fibre_context.clear_cancel_fn ctx : bool);
    x
  | exception ex ->
    ignore (Cancel.Fibre_context.clear_cancel_fn ctx : bool);
    match Promise.state cancelled with
    | `Fulfilled (Cancel.Cancelled ex2 as cex) when ex == ex2 ->
      (* We unwrapped the exception above to avoid a double cancelled exception.
         But this means that the top-level reported the original exception,
         which isn't what we want. *)
      raise cex
    | _ -> raise ex
