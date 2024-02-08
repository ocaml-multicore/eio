type pty = {
  masterfd : Unix.file_descr;
  slavefd : Unix.file_descr;
  name : string;
}

type pty_window = {
  row : int32;
  col : int32;
  xpixel : int32;
  ypixel : int32
}

external open_pty : unit -> pty = "eio_unix_open_pty"
external set_window_size : pty -> pty_window -> unit = "eio_unix_window_size"
external tty_window_size : unit -> pty_window = "eio_unix_tty_window_size"
