(* Copyright (C) 2014, Thomas Leonard *)

(* Note: we expect some kind of logger to process the trace buffer to collect
   events, but currently we don't have any barriers to ensure that the buffer
   is in a consistent state (although it usually is). So for now, you should
   pause tracing before trying to parse the buffer. In particular, GC events
   complicate things because we may need to add a GC event while in the middle
   of adding some other event. *)

include Eio_runtime_events

let current_thread = ref None

module Control = struct

  let event_log = ref false

  let stop () =
    match !event_log with
    | true -> event_log := false
    | _ -> failwith "Log is not currently tracing!"

  let start () = event_log := true

end

module RE = Eio_runtime_events

let log name =
  match !Control.event_log, !current_thread with
  | true, Some current_thread -> RE.note_log current_thread name
  | _ -> ()

let note_name id name =
  match !Control.event_log with
  | false -> ()
  | true -> RE.note_name id name

let set_name name =
    match !Control.event_log, !current_thread with
    | true, Some current_thread -> note_name current_thread name
    |  _-> ()

let note_created ?label id ty =
  match !Control.event_log with
  | false -> ()
  | true ->
    RE.note_created id ty;
    Option.iter (RE.note_name id) label

let note_parent ~child ~parent =
  match !Control.event_log with
  | false -> ()
  | true -> RE.note_parent ~child ~parent

let note_switch new_current =
  match !Control.event_log with
  | false -> ()
  | true ->
    current_thread := Some new_current;
    RE.note_switch new_current

let note_hiatus reason =
  match !Control.event_log with
  | false -> ()
  | true ->
    current_thread := None;
    RE.note_hiatus reason

let note_resume new_current =
  match !Control.event_log with
  | false -> ()
  | true -> RE.note_switch new_current

let note_try_read input =
  match !Control.event_log, !current_thread with
  | true, Some current_thread -> RE.note_try_read current_thread input
  | _ -> ()

let note_read ?reader input =
  match !Control.event_log with
  | false -> ()
  | true ->
    match reader, !current_thread with
    | None, None -> ()
    | Some reader, _ -> RE.note_read ~reader input
    | None, Some reader -> RE.note_read ~reader input

let note_resolved id ~ex =
  match !Control.event_log with
  | false -> ()
  | true -> RE.note_resolved id ~ex

let note_signal ?src dst =
  match !Control.event_log with
  | false -> ()
  | true ->
    match src, !current_thread with
    | None, None -> ()
    | Some src, _ -> RE.note_signal ~src dst
    | None, Some src -> RE.note_signal ~src dst
