include Tracing_common
module RE = Eio_runtime_events

let re_hiatus_of_eio_hiatus Wait_for_work = RE.Wait_for_work

let re_cc_of_eio_cc = function
  | Choose -> RE.Choose
  | Pick -> Pick
  | Join -> Join
  | Switch -> Switch
  | Protect -> Protect
  | Sub -> Sub
  | Root -> Root

let re_event_of_eio_event = function
  | Wait -> RE.Wait
  | Task -> Task
  | Bind -> Bind
  | Try -> Try
  | Map -> Map
  | Condition -> Condition
  | On_success -> On_success
  | On_failure -> On_failure
  | On_termination -> On_termination
  | On_any -> On_any
  | Ignore_result -> Ignore_result
  | Async -> Async
  | Promise -> Promise
  | Semaphore -> Semaphore
  | Switch -> Switch
  | Stream -> Stream
  | Mutex -> Mutex
  | Cancellation_context { purpose; protected } ->
    Cancellation_context {
      protected;
      purpose = re_cc_of_eio_cc purpose
    }
  | System_thread -> System_thread

module Control = struct

  let event_log = ref false

  let start () = event_log := true

  let stop () = event_log := false

  type ctf = unit

  let make ~timestamper:_ _ = ()

  let start_ctf () =
    Printf.eprintf "Ctf tracing is not supported after OCaml 5.1"

  let stop_ctf () = ()

end

let current_thread = ref None

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
    RE.note_created id (re_event_of_eio_event ty);
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
    RE.note_hiatus (re_hiatus_of_eio_hiatus reason)

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
