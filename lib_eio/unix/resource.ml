type 'a t = ([> `Unix_fd] as 'a) Eio.Resource.t

type ('t, _, _) Eio.Resource.pi += T : ('t, 't -> Fd.t, [> `Unix_fd]) Eio.Resource.pi
let fd (Eio.Resource.T (t, ops)) = Eio.Resource.get ops T t

let fd_opt (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops T with
  | Some f -> Some (f t)
  | None -> None
