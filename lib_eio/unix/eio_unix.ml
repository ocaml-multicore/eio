open Eio.Private.Effect

module Effects = struct
  type _ eff += 
    | Await_readable : Unix.file_descr -> unit eff
    | Await_writable : Unix.file_descr -> unit eff
end

let await_readable fd = perform (Effects.Await_readable fd)
let await_writable fd = perform (Effects.Await_writable fd)

module Ipaddr = struct
  let to_unix : _ Eio.Net.Ipaddr.t -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> _ Eio.Net.Ipaddr.t = Obj.magic
end
