(* This example shows how to trace an Eio program.

   The [main] function creates a listening socket and has a client connect and send a message,
   which is handled by a server fiber.

   At the same time, another fiber is displaying trace events.
   For simplicity, this example runs the tracer in the same process as the program being traced,
   but typically they would be separate processes. *)

open Eio.Std

let callbacks =
  Runtime_events.Callbacks.create ()
    (* Uncomment to trace GC events too: *)
(*
      ~runtime_begin:(handle (fun f phase -> Fmt.pf f "begin %s" (Runtime_events.runtime_phase_name phase)))
      ~runtime_end:(handle (fun f phase -> Fmt.pf f "end %s" (Runtime_events.runtime_phase_name phase)))
*)
    ~lost_events:(fun ring n -> traceln "ring %d lost %d events" ring n)
  |> Eio_runtime_events.add_callbacks
    (fun ring ts e ->
       (* Note: don't use traceln here, as it will just generate more log events! *)
       Fmt.epr "%9Ld:ring %d: %a@." (Runtime_events.Timestamp.to_int64 ts) ring Eio_runtime_events.pp_event e
    )
    (* (see lib_eio/runtime_events/eio_runtime_events.mli for more event types) *)

(* Read and display trace events from [cursor] until [finished]. *)
let trace ~finished (clock, delay) cursor =
  traceln "tracer: starting";
  let rec aux () =
    let _ : int = Runtime_events.read_poll cursor callbacks None in
    if !finished then (
      traceln "tracer: stopping"
    ) else (
      Eio.Time.Mono.sleep clock delay;
      aux ()
    )
  in
  aux ()

(* The program to be traced. *)
let main net =
  Switch.run ~name:"main" @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8123) in
  let s = Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net addr in
  Fiber.both
    (fun () ->
       traceln "server: starting";
       let c, _addr = Eio.Net.accept ~sw s in
       traceln "server: got connection from client";
       let msg = Eio.Flow.read_all c in
       traceln "server: read %S from socket" msg
    )
    (fun () ->
       traceln "client: connecting socket...";
       let c = Eio.Net.connect ~sw net addr in
       Eio.Flow.copy_string "Hello" c;
       Eio.Flow.close c
    )

(* Enable tracing then run the [main] and [trace] fibers. *)
let () =
  Runtime_events.start ();
  let cursor = Runtime_events.create_cursor None in    (* Create a in-process cursor *)
  Eio_main.run @@ fun env ->
  let finished = ref false in
  Fiber.both
    (fun () -> trace ~finished (env#mono_clock, 0.01) cursor)
    (fun () -> main env#net; finished := true)
