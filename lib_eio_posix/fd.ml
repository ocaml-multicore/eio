include Eio_unix.Fd

type has_fd = Eio_unix.Resource.t

let to_unix op t =
  match op with
  | `Take -> remove t |> Option.get
  | `Peek -> use_exn "to_unix" t Fun.id

let of_unix ~sw ~blocking ~close_unix fd = of_unix ~sw ~blocking ~close_unix fd

let get_fd_opt = Eio_unix.Resource.fd_opt
