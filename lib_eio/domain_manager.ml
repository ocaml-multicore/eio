open Std

type ty = [`Domain_mgr]
type 'a t = ([> ty] as 'a) r

module Pi = struct
  module type MGR = sig
    type t
    val run : t -> (cancelled:exn Promise.t -> 'a) -> 'a
    val run_raw : t -> (unit -> 'a) -> 'a
  end

  type (_, _, _) Resource.pi +=
    | Mgr : ('t, (module MGR with type t = 't), [> ty]) Resource.pi

  let mgr (type t) (module X : MGR with type t = t) =
    Resource.handler [H (Mgr, (module X))]
end

let run_raw (Resource.T (t, ops)) fn =
  let module X = (val (Resource.get ops Pi.Mgr)) in
  X.run_raw t fn

let run (Resource.T (t, ops)) fn =
  let module X = (val (Resource.get ops Pi.Mgr)) in
  X.run t @@ fun ~cancelled ->
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
