(* cp(1) built with effects. *)

open Fibreslib

module U = Eunix
module Int63 = Optint.Int63

let read_then_write_chunk infd outfd file_offset len =
  let buf = U.alloc () in
  Logs.debug (fun l -> l "r/w start %a (%d)" Int63.pp file_offset len);
  U.read_exactly ~file_offset infd buf len;
  U.write ~file_offset outfd buf len;
  Logs.debug (fun l -> l "r/w done  %a (%d)" Int63.pp file_offset len);
  U.free buf

let copy_file infd outfd insize block_size =
  let rec copy_block file_offset =
    let remaining = Int63.(sub insize file_offset) in
    if remaining <> Int63.zero then (
      let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
      let thread = U.fork (fun () -> read_then_write_chunk infd outfd file_offset len) in
      copy_block Int63.(add file_offset (of_int len));
      Promise.await thread
    )
  in
  copy_block Int63.zero

let run_cp block_size queue_depth infile outfile () =
  let open Unix in
  let infd = Eunix.openfile infile [O_RDONLY] 0 in
  let outfd = Eunix.openfile outfile [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let insize = Eunix.fstat infd |> fun {st_size; _} -> Int63.of_int st_size in
  Logs.debug (fun l -> l "eurcp: %s -> %s size %a queue %d bs %d"
                 infile
                 outfile
                 Int63.pp insize
                 queue_depth
                 block_size);
  U.run ~queue_depth ~block_size (fun () ->
      copy_file infd outfd insize block_size;
      Logs.debug (fun l -> l "eurcp: done");
      Eunix.FD.close outfd;
      Eunix.FD.close infd
    )
