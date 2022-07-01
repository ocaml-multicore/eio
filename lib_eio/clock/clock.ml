external system_clock : unit -> (int64 [@unboxed]) = 
  "caml_eio_system_clock" "caml_eio_system_clock_unboxed" [@@noalloc]
