(** Extension of {!Eio} for integration with OCaml's [Unix] module. *)

val await_readable : Unix.file_descr -> unit
(** [await_readable fd] blocks until [fd] is readable (or has an error). *)

val await_writable : Unix.file_descr -> unit
(** [await_writable fd] blocks until [fd] is writable (or has an error). *)

module Effects : sig
  open Eio.Private.Effect

  type _ eff += 
    | Await_readable : Unix.file_descr -> unit eff
    | Await_writable : Unix.file_descr -> unit eff
end
