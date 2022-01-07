open EffectHandlers

module Effects = struct
  type _ eff += 
    | Await_readable : Unix.file_descr -> unit eff
    | Await_writable : Unix.file_descr -> unit eff
end

let await_readable fd = perform (Effects.Await_readable fd)
let await_writable fd = perform (Effects.Await_writable fd)
