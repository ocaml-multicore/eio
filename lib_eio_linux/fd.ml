include Eio_unix.Fd

let is_open t =
  use t (fun _ -> true)
    ~if_closed:(fun () -> false)

let to_unix op t =
  match op with
  | `Take -> remove t |> Option.get
  | `Peek -> use_exn "to_unix" t Fun.id

let of_unix ~sw ~seekable ~close_unix fd = of_unix ~sw ~seekable ~close_unix fd
