
open Eio.Stdenv
open Eio
module Sync = Eio__Sync

let sender_fibers = 2

let message = 1234

(* Send [n_msgs] items to streams in a round-robin way. *)
let sender ~n_msgs streams =
  let msgs = Seq.take n_msgs (Seq.ints 0) in
  let streams = Seq.cycle (List.to_seq streams) in
  let zipped = Seq.zip msgs streams in
  ignore (Seq.iter (fun (_i, stream) ->
      Sync.put stream message) zipped)

(* Start one sender fiber for each stream, and let it send n_msgs messages.
   Each fiber sends to all streams in a round-robin way. *)
let run_senders ~dom_mgr ?(n_msgs = 100) streams =
  Switch.run @@ fun sw ->
  ignore @@ List.iter (fun _stream ->
      Fiber.fork ~sw (fun () ->
          Domain_manager.run dom_mgr (fun () ->
              sender ~n_msgs streams))) streams

(* Receive messages from all streams. *)
let receiver ~n_msgs streams =
  for _i = 1 to n_msgs do
    assert (Int.equal message (Sync.select_of_many streams));
  done

(* Create [n] streams. *)
let make_streams n =
  let unfolder i = if i == 0 then None else Some (Sync.create (), i-1) in
  let seq = Seq.unfold unfolder n in
  List.of_seq seq

let run env =
  let dom_mgr = domain_mgr env in
  let clock = clock env in
  let streams = make_streams sender_fibers in
  let selector = List.map (fun s -> (s, fun i -> i)) streams in
  let n_msgs = 10000 in
  Switch.run @@ fun sw ->
  Fiber.fork ~sw (fun () -> run_senders ~dom_mgr ~n_msgs streams);
  let before = Time.now clock in
  receiver ~n_msgs:(sender_fibers * n_msgs) selector;
  let after = Time.now clock in
  let elapsed = after -. before in
  let time_per_iter = elapsed /. (Float.of_int @@ sender_fibers * n_msgs) in
  [Metric.create
     (Printf.sprintf "sync:true senders:%d msgs_per_sender:%d" sender_fibers n_msgs)
     (`Float (1e9 *. time_per_iter)) "ns"
     "Time per transmitted int"]


