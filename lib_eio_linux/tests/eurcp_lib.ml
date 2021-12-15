(* cp(1) built with effects. *)

open Eio.Std

module U = Eio_linux
module Int63 = Optint.Int63

let read_then_write_chunk infd outfd file_offset len =
  let buf = U.alloc () in
  Logs.debug (fun l -> l "r/w start %a (%d)" Int63.pp file_offset len);
  U.read_exactly ~file_offset infd buf len;
  U.write ~file_offset outfd buf len;
  Logs.debug (fun l -> l "r/w done  %a (%d)" Int63.pp file_offset len);
  U.free buf

let copy_file infd outfd insize block_size =
  Switch.run @@ fun sw ->
  let rec copy_block file_offset =
    let remaining = Int63.(sub insize file_offset) in
    if remaining <> Int63.zero then (
      let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
      Fibre.fork ~sw (fun () -> read_then_write_chunk infd outfd file_offset len);
      copy_block Int63.(add file_offset (of_int len))
    )
  in
  copy_block Int63.zero

let run_cp block_size queue_depth infile outfile () =
  U.run ~queue_depth ~block_size @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let open Unix in
  let infd = Eio_linux.openfile ~sw infile [O_RDONLY] 0 in
  let outfd = Eio_linux.openfile ~sw outfile [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let insize = Eio_linux.fstat infd |> fun {st_size; _} -> Int63.of_int st_size in
  Logs.debug (fun l -> l "eurcp: %s -> %s size %a queue %d bs %d"
                 infile
                 outfile
                 Int63.pp insize
                 queue_depth
                 block_size);
  copy_file infd outfd insize block_size;
  Logs.debug (fun l -> l "eurcp: done")
