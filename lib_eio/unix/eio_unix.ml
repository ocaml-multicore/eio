open Eio.Private.Effect

module Private = struct
  type _ Eio.Generic.ty += Unix_file_descr : [`Peek | `Take] -> Unix.file_descr Eio.Generic.ty

  type _ eff += 
    | Await_readable : Unix.file_descr -> unit eff
    | Await_writable : Unix.file_descr -> unit eff
end

let await_readable fd = perform (Private.Await_readable fd)
let await_writable fd = perform (Private.Await_writable fd)

module FD = struct
  let peek x = Eio.Generic.probe x (Private.Unix_file_descr `Peek)
  let take x = Eio.Generic.probe x (Private.Unix_file_descr `Take)
end

module Ipaddr = struct
  let to_unix : _ Eio.Net.Ipaddr.t -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> _ Eio.Net.Ipaddr.t = Obj.magic
end
