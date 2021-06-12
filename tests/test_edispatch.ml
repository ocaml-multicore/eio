open Fibreslib

let () =
  Logs.(set_level ~all:true (Some Debug));
  Logs.set_reporter @@ Logs.format_reporter ();
  Printexc.record_backtrace true

let state t =
  Alcotest.of_pp
    (fun f -> function
       | `Unresolved -> Fmt.string f "unresolved"
       | `Broken (Failure msg) -> Fmt.pf f "broken:%s" msg
       | `Broken ex -> Fmt.pf f "broken:%a" Fmt.exn ex
       | `Fulfilled x -> Fmt.pf f "fulfilled:%a" (Alcotest.pp t) x
    )

let get_state p =
  match Promise.state p with
  | Unresolved _ -> `Unresolved
  | Broken ex -> `Broken ex
  | Fulfilled x -> `Fulfilled x

let test_write () =
  Eio_main.run @@ fun _stdenv ->
  let msg = "Hello!" in
  let buffer = Buffer.create 20 in
  Eio.Flow.write (Eio.Flow.buffer_sink buffer) ~src:(Eio.Flow.string_source msg);
  Alcotest.(check string) "Copy correct" (msg) (Buffer.contents buffer)

(* Write a string to a pipe and read it out again. *)
let test_copy () =
  Edispatch.run @@ fun _stdenv ->
  Switch.top @@ fun sw ->
  let msg = "Hello!" in
  let from_pipe, to_pipe = Edispatch.pipe () in
  let buffer = Buffer.create 20 in
  Fibre.both ~sw
    (fun () -> Eio.Flow.write (Eio.Flow.buffer_sink buffer) ~src:from_pipe)
    (fun () ->
       Eio.Flow.write to_pipe ~src:(Eio.Flow.string_source msg);
       Eio.Flow.write to_pipe ~src:(Eio.Flow.string_source msg);
       Eio.Flow.close to_pipe
    );
  Alcotest.(check string) "Copy correct" (msg ^ msg) (Buffer.contents buffer);
  Eio.Flow.close from_pipe

(* Tests the fast_copy code path (Dispatch.Data.t -> Dispatch.Data.t) *)
let test_direct_copy () =
  Edispatch.run @@ fun _stdenv ->
  let msg = "Hello!" in
  let from_pipe1, to_pipe1 = Edispatch.pipe () in
  let from_pipe2, to_pipe2 = Edispatch.pipe () in
  let buffer = Buffer.create 20 in
  let to_output = Eio.Flow.buffer_sink buffer in
  Switch.top (fun sw ->
      Fibre.fork_ignore ~sw (fun () -> Ctf.label "copy1"; Eio.Flow.write ~src:from_pipe1 to_pipe2; Eio.Flow.close to_pipe2);
      Fibre.fork_ignore ~sw (fun () -> Ctf.label "copy2"; Eio.Flow.write ~src:from_pipe2 to_output);
      Eio.Flow.write to_pipe1 ~src:(Eio.Flow.string_source msg);
      Eio.Flow.close to_pipe1;
    );
  Alcotest.(check string) "Copy correct" msg (Buffer.contents buffer);
  Eio.Flow.close from_pipe1;
  Eio.Flow.close from_pipe2

let () =
  let open Alcotest in
  run "eio-with-edispatch" [
    "io", [
      test_case "write" `Quick test_write;
      test_case "copy" `Quick test_copy;
      test_case "direct_copy" `Quick test_direct_copy;
    ];
  ]