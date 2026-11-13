open Eio.Std

val listening_socket :
  ?hook:Eio__core.Switch.hook ->
  Eio_unix.Fd.t -> [ [`Unix | `Generic] Eio.Net.listening_socket_ty | `Unix_fd ] r                                                                       
val datagram_socket :
  ?hook:Eio__core.Switch.hook ->
  Eio_unix.Fd.t -> [ [`Unix | `Generic] Eio.Net.datagram_socket_ty | `Unix_fd ] r

val v : [`Unix | `Generic] Eio.Net.ty r
