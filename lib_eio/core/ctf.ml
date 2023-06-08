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

let note_loc id name =
  match !Control.event_log with
  | false -> ()
  | true -> RE.note_location id name

let set_name name =
    match !Control.event_log, !current_thread with
    | true, Some current_thread -> note_name current_thread name
    |  _-> ()

let set_loc name =
  match !Control.event_log, !current_thread with
  | true, Some current_thread -> note_loc current_thread name
  |  _-> ()

let note_created ?label ?loc id ty =
  match !Control.event_log with
  | false -> ()
  | true ->
    RE.note_created id ty;
    Option.iter (RE.note_name id) label;
    Option.iter (RE.note_location id) loc

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

let demangle x = List.flatten (
  List.map (fun i ->
    Astring.String.cuts ~sep:"__" i
    |> List.fold_left (fun a b -> match (a, b) with
      | [], b -> [b]
      | v, "" -> v
      | a::v, s when Astring.Char.Ascii.is_lower s.[0] -> (a^"_"^s) :: v
      | v, s -> s :: v) []
    |> List.rev ) x
  )

let is_outer raw_entry =
  let slot = Printexc.backtrace_slots_of_raw_entry raw_entry in
  match slot with
  | None -> None
  | Some slots ->
    Array.find_map (fun slot ->
      let (let*) = Option.bind in
      let* loc = Printexc.Slot.location slot in
      let* name = Printexc.Slot.name slot in
      let* name = match String.split_on_char '.' name |> demangle with
        | "Eio_core" :: _ -> None
        | "Eio" :: _ -> None
        | "Eio_linux" :: _ -> None
        | "Eio_luv" :: _ -> None
        | "Eio_main" :: _ -> None
        | "Stdlib" :: _ -> None
        | "Dune_exe" :: v -> Some (String.concat "." v)
        | v -> Some (String.concat "." v)
      in
      Some (Fmt.str "%s (%s:%d)" name loc.filename loc.line_number)
    ) slots

let dune_exe_strategy stack =
  let first acc s = match acc with
    | (Some _ as v) -> v
    | _ -> is_outer s
  in
  List.fold_left first None stack

let get_caller () =
  let p = Printexc.get_callstack 30 |> Printexc.raw_backtrace_to_string in
  let stack = Printexc.get_callstack 30 |> Printexc.raw_backtrace_entries |> Array.to_list in
  match dune_exe_strategy stack with
  | Some v -> v
  | None -> p
