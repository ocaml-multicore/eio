
open Eio.Stdenv
open Eio
module Sync = Eio__Sync

let sender_fibers = 10

let message = 1234

let sender ~id ~n_msgs stream =
  for i = 1 to n_msgs do
    traceln "Sent message #%d from %d" i id;
    Sync.put stream message
  done

(* Start one sender fiber for each stream, and let it send n_msgs messages. *)
let run_senders ~dom_mgr ?(n_msgs = 100) streams =
  let count = ref 0 in
  Switch.run @@ fun sw ->
    ignore @@ List.map (fun stream ->
      Fiber.fork ~sw (fun () ->
        let id = !count in
        count := !count + 1;
        Domain_manager.run dom_mgr (fun () ->
          sender ~id ~n_msgs stream))) streams

let receiver ~n_msgs streams =
  for i = 1 to n_msgs do
    assert (Int.equal message (Sync.select_of_many streams));
    traceln "Received message #%d" i
  done

let make_streams n =
  let unfolder i = if i == 0 then None else Some (Sync.create (), i-1) in
  let seq = Seq.unfold unfolder n in
  List.of_seq seq

(* Currently fails with exception from ocaml-uring/lib/uring/uring.ml:326
    https://github.com/ocaml-multicore/ocaml-uring/blob/07482dae72c8e977e4e4e2b2c8bd137e770ee1dd/lib/uring/uring.ml#L327
*)
let run env =
  let dom_mgr = domain_mgr env in
  let streams = make_streams sender_fibers in
  let n_msgs = 50 in
  Switch.run @@ fun sw ->
    Fiber.fork ~sw (fun () -> run_senders ~dom_mgr ~n_msgs streams);
    receiver ~n_msgs:(sender_fibers * n_msgs) streams;
    []


