open Eio.Std

type t = {
  pty : [Types.source_ty | Types.sink_ty] r;
  tty : Fd.t;
  name : string;
}

external create : unit -> Unix.file_descr = "eio_unix_open_pty"

(* Not domain-safe on platforms without [ptsname_r] *)
external get_pty_peer : Unix.file_descr -> Unix.file_descr * string = "eio_unix_get_pty_peer"

let open_pty ~sw () =
  let pty_fd = create () in
  Unix.set_nonblock pty_fd;
  let pty = Net.import_socket_stream ~sw ~close_unix:true pty_fd in
  let tty_fd, name = get_pty_peer pty_fd in
  let tty = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true tty_fd in
  { pty; tty; name }

let pty t = Resource.fd t.pty
let tty t = t.tty
let name t = t.name

let source t = (t.pty :> Types.source_ty r)
let sink t = (t.pty :> Types.sink_ty r)

type winsize = {
  rows : int;
  cols : int;
  xpixel : int;
  ypixel : int;
}

external get_winsize : Unix.file_descr -> winsize = "eio_unix_get_winsize"
external set_winsize : Unix.file_descr -> winsize -> unit = "eio_unix_set_winsize"

let get_window_size fd = Fd.use_exn "get_window_size" fd get_winsize
let set_window_size fd ws = Fd.use_exn "set_window_size" fd (fun fd -> set_winsize fd ws)
