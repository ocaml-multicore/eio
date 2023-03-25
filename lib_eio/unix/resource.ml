type t = < fd : Fd.t >

type _ Eio.Generic.ty += FD : Fd.t Eio.Generic.ty

let fd t = t#fd
let fd_opt t = Eio.Generic.probe t FD
