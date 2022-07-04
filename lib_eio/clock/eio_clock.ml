class virtual t = object
  method virtual now : float
  method virtual now_ns : int64
  method virtual sleep_until : float -> unit
end

external system_clock : unit -> (int64 [@unboxed]) = 
  "caml_eio_system_clock" "caml_eio_system_clock_unboxed" [@@noalloc]

external mono_clock: unit -> (int64 [@unboxed]) = 
  "caml_eio_mono_clock" "caml_eio_mono_clock_unboxed" [@@noalloc]

external ns_to_seconds: int64 -> (float [@unboxed]) = 
  "caml_eio_ns_to_seconds" "caml_eio_ns_to_seconds_unboxed" [@@noalloc]
