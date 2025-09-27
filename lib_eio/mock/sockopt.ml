open Eio.Std

let setsockopt label opt v = traceln "%s: setsockopt %a" label Eio.Net.Sockopt.pp_binding (opt, v)
let getsockopt _label _opt = raise (Eio.Net.err Invalid_option)
