(* cp(1) built with effects. *)

module U = Eunix

let read_then_write_chunk infd outfd file_offset len =
  let buf = U.alloc () in
  Logs.debug (fun l -> l "r/w start %d (%d)" file_offset len);
  U.read ~file_offset infd buf len;
  U.write ~file_offset outfd buf len;
  Logs.debug (fun l -> l "r/w done  %d (%d)" file_offset len);
  U.free buf

let copy_file infd outfd insize block_size =
  let rec copy_block file_offset =
    match insize - file_offset with
    | 0 -> ()
    | remaining ->
       let len = min block_size remaining in
       U.fork (fun () -> read_then_write_chunk infd outfd file_offset len);
       copy_block (file_offset + len)
  in
  copy_block 0

let run_cp block_size queue_depth infile outfile () =
  let open Unix in
  let infd = openfile infile [O_RDONLY] 0 in
  let outfd = openfile outfile [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let insize = fstat infd |> fun {st_size; _} -> st_size in
  Logs.debug (fun l -> l "eurcp: %s -> %s size %d queue %d bs %d"
    infile outfile insize queue_depth block_size);
  U.run ~queue_depth ~block_size (fun () -> copy_file infd outfd insize block_size);
  Logs.debug (fun l -> l "eurcp: done");
  close outfd;
  close infd
