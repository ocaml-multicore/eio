open Bigarray

module Tracing = Eio.Private.Tracing

let timestamper log_buffer ofs =
  let ns = Mtime.to_uint64_ns @@ Mtime_clock.now () in
  Tracing.BS.set_int64_le log_buffer ofs ns

let mmap_buffer ~size path =
  let fd = Unix.(openfile path [O_RDWR; O_CREAT; O_TRUNC] 0o644) in
  Unix.set_close_on_exec fd;
  Unix.ftruncate fd size;
  let ba = array1_of_genarray (Unix.map_file fd char c_layout true [| size |]) in
  Unix.close fd;
  ba

let with_tracing ?(size=0x100000) path fn =
  let buffer = mmap_buffer ~size path in
  let trace_config = Tracing.Control.make ~timestamper buffer in
  Tracing.Control.start_ctf trace_config;
  Fun.protect fn ~finally:(fun () -> Tracing.Control.stop_ctf trace_config)
