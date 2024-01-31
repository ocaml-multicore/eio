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

let create_obj ?label id ty =
  add_event RE.create_obj (id, ty);
  Option.iter (fun l -> add_event RE.name (id, l)) label

let create_cc id ty =
  add_event RE.create_cc (id, ty)

let create_fiber ~cc id =
  add_event RE.create_fiber (id, cc)

let log = add_event RE.log
let name id x = add_event RE.name (id, x)
let enter_span = add_event RE.enter_span
let exit_span = add_event RE.exit_span
let fiber = add_event RE.fiber
let suspend_domain = add_event RE.suspend_domain
let try_get = add_event RE.try_get
let get = add_event RE.get
let put = add_event RE.put
let exit_fiber = add_event RE.exit_fiber
let exit_cc = add_event RE.exit_cc
let error id ex = add_event RE.error (id, ex)
let suspend_fiber op = add_event RE.suspend_fiber op
let domain_spawn ~parent = add_event RE.domain_spawn parent

let with_span op fn =
  enter_span op;
  match fn () with
  | r -> exit_span (); r
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    exit_span ();
    Printexc.raise_with_backtrace ex bt
