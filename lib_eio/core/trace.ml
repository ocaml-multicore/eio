(* Copyright (C) 2014, Thomas Leonard *)

type id = int

let id_chunk_size = 1024

let next_id_chunk = Atomic.make 0

let next_id_key =
  Domain.DLS.new_key (fun () -> Atomic.fetch_and_add next_id_chunk id_chunk_size)

let mint_id () =
  let next_id_local = Domain.DLS.get next_id_key in
  let next_id_local_succ =
    if ((next_id_local + 1) mod id_chunk_size) = 0 then
      (* we're out of local IDs *)
      Atomic.fetch_and_add next_id_chunk id_chunk_size
    else
      next_id_local + 1
  in
  Domain.DLS.set next_id_key next_id_local_succ;
  next_id_local

module RE = Eio_runtime_events

let add_event = Runtime_events.User.write

let create ?label id ty =
  add_event RE.create (id, ty);
  Option.iter (fun l -> add_event RE.name (id, l)) label

let log = add_event RE.log
let fiber = add_event RE.fiber
let suspend = add_event RE.suspend
let try_read = add_event RE.try_read
let read = add_event RE.read
let signal = add_event RE.signal
let resolve = add_event RE.resolve
let resolve_error id ex = add_event RE.resolve_error (id, ex)
