(* cp(1) built with effects. *)

open Eio.Std

module U = Eio_linux
module Int63 = Optint.Int63

let fibres = Eio.Semaphore.make 64

(* Fill [buf] from [infd], starting the read at [file_offset]. *)
let rec read_exactly ~file_offset infd buf =
  let got = U.readv ~file_offset infd [buf] in
  if got <> Cstruct.length buf then
    read_exactly ~file_offset:(Int63.add file_offset (Int63.of_int got)) infd (Cstruct.shift buf got)

(* Various copy techniques *)
module Copy_chunk = struct

  (* Use the special fixed buffer operations for the copy (the original version) *)
  let fixed_buffer infd outfd file_offset len =
    let buf = U.alloc () in
    U.read_exactly ~file_offset infd buf len;
    U.write ~file_offset outfd buf len;
    U.free buf

  (* Allocate a fresh cstruct and use that with the plain [readv]/[writev]. *)
  let new_cstruct infd outfd file_offset len =
    let buf = Cstruct.create_unsafe len in
    read_exactly ~file_offset infd buf;
    U.writev ~file_offset outfd [buf]

  (* Use the region allocator to get the buffer, but then just use it as a regular cstruct. *)
  let chunk_as_cstruct infd outfd file_offset len =
    let chunk = U.alloc () in
    let buf = Uring.Region.to_cstruct chunk ~len in
    read_exactly ~file_offset infd buf;
    U.writev ~file_offset outfd [buf];
    U.free chunk

end

let copy_file ~copy_chunk infd outfd insize block_size =
  Switch.run @@ fun sw ->
  let rec copy_block file_offset =
    let remaining = Int63.(sub insize file_offset) in
    if remaining <> Int63.zero then (
      let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
      Eio.Semaphore.acquire fibres;
      Fibre.fork_ignore ~sw (fun () ->
          Logs.debug (fun l -> l "r/w start %a (%d)" Int63.pp file_offset len);
          copy_chunk infd outfd file_offset len;
          Logs.debug (fun l -> l "r/w done  %a (%d)" Int63.pp file_offset len);
          Eio.Semaphore.release fibres
        );
      copy_block Int63.(add file_offset (of_int len))
    )
  in
  copy_block Int63.zero

let run_cp ~copy_chunk block_size queue_depth infile outfile () =
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
  copy_file ~copy_chunk infd outfd insize block_size;
  Logs.debug (fun l -> l "eurcp: done")

let run_cp block_size queue_depth infile outfile () =
  U.run ~queue_depth ~block_size @@ fun _stdenv ->
  let test name copy_chunk =
    Unix.system "sync" |> ignore;
    Gc.full_major ();
    let t0 = Unix.gettimeofday () in
    run_cp ~copy_chunk block_size queue_depth infile outfile ();
    let t1 = Unix.gettimeofday () in
    Printf.printf "%16s, %.3f\n%!" name (t1 -. t0)
  in
  print_endline "          method, time/s";
  for _ = 1 to 5 do
    test "fixed-buffer" Copy_chunk.fixed_buffer;
    test "new-cstruct" Copy_chunk.new_cstruct;
    test "chunk-as-cstruct" Copy_chunk.chunk_as_cstruct;
  done
