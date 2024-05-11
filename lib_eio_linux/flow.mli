open Eio.Std

val of_fd : Eio_unix.Fd.t -> [< `Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r

val stdin : Eio_unix.source_ty r
val stdout : Eio_unix.sink_ty r
val stderr : Eio_unix.sink_ty r

val secure_random : Eio.Flow.source_ty r
