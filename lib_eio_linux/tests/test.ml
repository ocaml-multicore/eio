open Eio.Std

module Trace = Eio.Private.Trace

let () =
  Logs.(set_level ~all:true (Some Debug));
  Logs.set_reporter @@ Logs.format_reporter ();
  Printexc.record_backtrace true

let read_one_byte ~sw r =
  Fiber.fork_promise ~sw (fun () ->
      let r = Eio_unix.Resource.fd r in
      Eio_linux.Low_level.await_readable r;
      Eio_unix.Fd.use_exn "read" r @@ fun r ->
      let b = Bytes.create 1 in
      let got = Unix.read r b 0 1 in
      assert (got = 1);
      Bytes.to_string b
    )

let test_poll_add () =
  Eio_linux.run @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let thread = read_one_byte ~sw r in
  Fiber.yield ();
  let w = Eio_unix.Resource.fd w in
  Eio_linux.Low_level.await_writable w;
  let sent =
    Eio_unix.Fd.use_exn "write" w @@ fun w ->
    Unix.write w (Bytes.of_string "!") 0 1 in
  assert (sent = 1);
  let result = Promise.await_exn thread in
  Alcotest.(check string) "Received data" "!" result

let test_poll_add_busy () =
  Eio_linux.run ~queue_depth:2 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let a = read_one_byte ~sw r in
  let b = read_one_byte ~sw r in
  Fiber.yield ();
  let w = Eio_unix.Resource.fd w in
  let sent =
    Eio_unix.Fd.use_exn "write" w @@ fun w ->
    Unix.write w (Bytes.of_string "!!") 0 2
  in
  assert (sent = 2);
  let a = Promise.await_exn a in
  Alcotest.(check string) "Received data" "!" a;
  let b = Promise.await_exn b in
  Alcotest.(check string) "Received data" "!" b

(* Write a string to a pipe and read it out again. *)
let test_copy () =
  Eio_linux.run ~queue_depth:3 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe, to_pipe = Eio_unix.pipe sw in
  let buffer = Buffer.create 20 in
  Fiber.both
    (fun () -> Eio.Flow.copy from_pipe (Eio.Flow.buffer_sink buffer))
    (fun () ->
       Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe;
       Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe;
       Eio.Flow.close to_pipe
    );
  Alcotest.(check string) "Copy correct" (msg ^ msg) (Buffer.contents buffer);
  Eio.Flow.close from_pipe

(* Write a string via 2 pipes. The copy from the 1st to 2nd pipe will be optimised and so tests a different code-path. *)
let test_direct_copy () =
  Eio_linux.run ~queue_depth:4 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe1, to_pipe1 = Eio_unix.pipe sw in
  let from_pipe2, to_pipe2 = Eio_unix.pipe sw in
  let buffer = Buffer.create 20 in
  let to_output = Eio.Flow.buffer_sink buffer in
  Switch.run (fun sw ->
      Fiber.fork ~sw (fun () -> Trace.log "copy1"; Eio.Flow.copy from_pipe1 to_pipe2; Eio.Flow.close to_pipe2);
      Fiber.fork ~sw (fun () -> Trace.log "copy2"; Eio.Flow.copy from_pipe2 to_output);
      Eio.Flow.copy (Eio.Flow.string_source msg) to_pipe1;
      Eio.Flow.close to_pipe1;
    );
  Alcotest.(check string) "Copy correct" msg (Buffer.contents buffer);
  Eio.Flow.close from_pipe1;
  Eio.Flow.close from_pipe2

(* Read and write using IO vectors rather than the fixed buffers. *)
let test_iovec () =
  Eio_linux.run ~queue_depth:4 @@ fun _stdenv ->
  Switch.run @@ fun sw ->
  let from_pipe, to_pipe = Eio_unix.pipe sw in
  let from_pipe = Eio_unix.Resource.fd from_pipe in
  let to_pipe = Eio_unix.Resource.fd to_pipe in
  let message = Cstruct.of_string "Got [   ] and [   ]" in
  let rec recv = function
    | [] -> ()
    | cs ->
      let got = Eio_linux.Low_level.readv from_pipe cs in
      recv (Cstruct.shiftv cs got)
  in
  Fiber.both
    (fun () -> recv [Cstruct.sub message 5 3; Cstruct.sub message 15 3])
    (fun () ->
       let b = Cstruct.of_string "barfoo" in
       Eio_linux.Low_level.writev to_pipe [Cstruct.sub b 3 3; Cstruct.sub b 0 3];
       Eio_unix.Fd.close to_pipe
    );
  Alcotest.(check string) "Transfer correct" "Got [foo] and [bar]" (Cstruct.to_string message)

(* We fill the SQE buffer and need to submit early. *)
let test_no_sqe () =
  try
    Eio_linux.run ~queue_depth:4 @@ fun _stdenv ->
    Switch.run @@ fun sw ->
    for _ = 1 to 8 do
      Fiber.fork ~sw (fun () ->
          let r, _w = Eio_unix.pipe sw in
          ignore (Eio.Flow.single_read r (Cstruct.create 1) : int);
          assert false
        )
    done;
    raise Exit
  with Exit -> ()

let test_read_exact () =
  Eio_linux.run ~queue_depth:4 ~n_blocks:1 @@ fun env ->
  let ( / ) = Eio.Path.( / ) in
  let path = env#cwd / "test.data" in
  let msg = "hello" in
  Eio.Path.save path ("!" ^ msg) ~create:(`Or_truncate 0o600);
  Switch.run @@ fun sw ->
  let fd = Eio_linux.Low_level.openat2 ~sw
    ~access:`R
    ~flags:Uring.Open_flags.empty
    ~perm:0
    ~resolve:Uring.Resolve.empty
    "test.data"
  in
  Eio_linux.Low_level.with_chunk ~fallback:Alcotest.skip @@ fun chunk ->
  (* Try to read one byte too far. If it's not updating the file offset, it will
     succeed. *)
  let len = String.length msg + 1 in
  try
    Eio_linux.Low_level.read_exactly ~file_offset:Optint.Int63.one fd chunk len;
    assert false
  with End_of_file ->
    let got = Uring.Region.to_string chunk ~len:(String.length msg) in
    if got <> msg then Fmt.failwith "%S vs %S" got msg

let test_expose_backend () =
  Eio_linux.run @@ fun env ->
  let backend = Eio.Stdenv.backend_id env in
  assert (backend = "linux")

let kind_t = Alcotest.of_pp Uring.Statx.pp_kind

let test_statx () =
  let module X = Uring.Statx in
  Eio_linux.run ~queue_depth:4 @@ fun env ->
  let ( / ) = Eio.Path.( / ) in
  let path = env#cwd / "test2.data" in
  Eio.Path.with_open_out path ~create:(`Or_truncate 0o600) @@ fun file ->
  Eio.Flow.copy_string "hello" file;
  let buf = Uring.Statx.create () in
  let test expected_len ~follow dir path =
    Eio_linux.Low_level.statx ~follow ~mask:X.Mask.(type' + size) dir path buf;
    Alcotest.check kind_t "kind" `Regular_file (Uring.Statx.kind buf);
    Alcotest.(check int64) "size" expected_len (Uring.Statx.size buf)
  in
  (* Lookup via cwd *)
  test 5L ~follow:false Cwd "test2.data";
  Eio.Flow.copy_string "+" file;
  (* Lookup via file FD *)
  Switch.run (fun sw ->
      let fd = Eio_linux.Low_level.openat2 ~sw
          ~access:`R
          ~flags:Uring.Open_flags.empty
          ~perm:0
          ~resolve:Uring.Resolve.empty
          "test2.data"
      in
      test 6L ~follow:false (FD fd) ""
    );
  (* Lookup via directory FD *)
  Eio.Flow.copy_string "+" file;
  Switch.run (fun sw ->
      let fd = Eio_linux.Low_level.openat2 ~sw
          ~access:`R
          ~flags:Uring.Open_flags.path
          ~perm:0
          ~resolve:Uring.Resolve.empty
          "."
      in
      test 7L ~follow:false (FD fd) "test2.data"
    );
  ()

(* Ensure that an OCaml signal handler will run, even if we're sleeping in liburing at the time.
   The problem here is that [__sys_io_uring_enter2] doesn't return EINTR, because it did successfully
   submit an item. This causes liburing to retry without giving our OCaml signal handler a chance to run.
   Note: we can't run this test with a timeout because liburing does return in that case! *)
let test_signal_race () =
  Eio_linux.run @@ fun _env ->
  let cond = Eio.Condition.create () in
  let handle _ = Eio.Condition.broadcast cond in
  Sys.(set_signal sigalrm) (Signal_handle handle);
  Fiber.both
    (fun () -> Eio.Condition.await_no_mutex cond)
    (fun () -> ignore (Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0.001 } : Unix.interval_timer_status))

let test_alloc_fixed_or_wait () =
  Eio_linux.run ~n_blocks:1 @@ fun _env ->
  let block = Eio_linux.Low_level.alloc_fixed_or_wait () in
  (* We have to wait for the block, but get cancelled while waiting. *)
  begin
    try
      Fiber.both
        (fun () -> ignore (Eio_linux.Low_level.alloc_fixed_or_wait () : Uring.Region.chunk))
        (fun () -> raise Exit);
    with Exit -> ()
  end;
  (* We have to wait for the block, and get it when the old one is freed. *)
  Fiber.both
    (fun () ->
       let x = Eio_linux.Low_level.alloc_fixed_or_wait () in
       Eio_linux.Low_level.free_fixed x
    )
    (fun () ->
       Eio_linux.Low_level.free_fixed block
    );
  (* The old block is passed to the waiting fiber, but it's cancelled. *)
  let block = Eio_linux.Low_level.alloc_fixed_or_wait () in
  Fiber.both
    (fun () ->
       Fiber.first
         (fun () -> ignore (Eio_linux.Low_level.alloc_fixed_or_wait ()); assert false)
         (fun () -> ())
    )
    (fun () -> Eio_linux.Low_level.free_fixed block);
  let block = Eio_linux.Low_level.alloc_fixed_or_wait () in
  Eio_linux.Low_level.free_fixed block

let () =
  let open Alcotest in
  run "eio_linux" [
    "io", [
      test_case "copy"                 `Quick test_copy;
      test_case "direct_copy"          `Quick test_direct_copy;
      test_case "poll_add"             `Quick test_poll_add;
      test_case "poll_add_busy"        `Quick test_poll_add_busy;
      test_case "iovec"                `Quick test_iovec;
      test_case "no_sqe"               `Quick test_no_sqe;
      test_case "read_exact"           `Quick test_read_exact;
      test_case "expose_backend"       `Quick test_expose_backend;
      test_case "statx"                `Quick test_statx;
      test_case "signal_race"          `Quick test_signal_race;
      test_case "alloc-fixed-or-wait"  `Quick test_alloc_fixed_or_wait;
    ];
  ]
