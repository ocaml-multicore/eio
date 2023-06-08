(* ID allocation *)

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

(* Event types *)

type hiatus_reason = Wait_for_work

type cancellation_context =
  | Choose
  | Pick
  | Join
  | Switch
  | Protect
  | Sub
  | Root

type event =
  | Wait
  | Task
  | Bind
  | Try
  | Map
  | Condition
  | On_success
  | On_failure
  | On_termination
  | On_any
  | Ignore_result
  | Async
  | Promise
  | Semaphore
  | Switch
  | Stream
  | Mutex
  | Cancellation_context of {
    purpose: cancellation_context;
    protected: bool;
  }
  | System_thread

let int_of_cc_type = function
  | Choose -> 0
  | Pick -> 1
  | Join -> 2
  | Switch -> 3
  | Protect -> 4
  | Sub -> 5
  | Root -> 6

let serialize_thread_type ~ofs buf t =
  let id =
    match t with
    | Wait -> 0
    | Task -> 1
    | Bind -> 2
    | Try -> 3
    | Map -> 7
    | Condition -> 8
    | On_success -> 9
    | On_failure -> 10
    | On_termination -> 11
    | On_any -> 12
    | Ignore_result -> 13
    | Async -> 14
    | Promise -> 15
    | Semaphore -> 16
    | Switch -> 17
    | Stream -> 18
    | Mutex -> 19
    | Cancellation_context _ -> 20
    | System_thread -> 21
  in
  Bytes.set_int8 buf ofs id;
  match t with
  | Cancellation_context {protected; purpose} ->
    Bytes.set_int8 buf (ofs+1) (int_of_cc_type purpose);
    Bytes.set_int8 buf (ofs+2) (Bool.to_int protected);
    3
  | _ -> 1

let cc_to_string = function
  | Choose -> "choose"
  | Pick -> "pick"
  | Join -> "join"
  | Switch -> "switch"
  | Protect -> "protect"
  | Sub -> "sub"
  | Root -> "root"

let event_to_string (t : event) =
  match t with
  | Wait -> "wait"
  | Task -> "task"
  | Bind -> "bind"
  | Map -> "map"
  | Try -> "try"
  | Condition -> "condition"
  | On_success -> "on-success"
  | On_failure -> "on-failure"
  | On_termination -> "on-termination"
  | On_any -> "on-any"
  | Ignore_result -> "ignore-result"
  | Async -> "async"
  | Promise -> "promise"
  | Semaphore -> "semaphore"
  | Switch -> "switch"
  | Stream -> "stream"
  | Mutex -> "mutex"
  | Cancellation_context {purpose; _} ->
    "cancellation-context("^ (cc_to_string purpose) ^")"
  | System_thread -> "system-thread"

let int_to_cc_type = function
  | 0 -> Choose
  | 1 -> Pick
  | 2 -> Join
  | 3 -> Switch
  | 4 -> Protect
  | 5 -> Sub
  | 6 -> Root
  | _ -> assert false

let parse_thread_type ~ofs buf = match Bytes.get_int8 buf ofs with
  | 0 -> Wait
  | 1 -> Task
  | 2 -> Bind
  | 3 -> Try
  | 7 -> Map
  | 8 -> Condition
  | 9 -> On_success
  | 10 -> On_failure
  | 11 -> On_termination
  | 12 -> On_any
  | 13 -> Ignore_result
  | 14 -> Async
  | 15 -> Promise
  | 16 -> Semaphore
  | 17 -> Switch
  | 18 -> Stream
  | 19 -> Mutex
  | 20 ->
    let purpose = Bytes.get_int8 buf (ofs+1) |> int_to_cc_type in
    let protected = Bytes.get_int8 buf (ofs+2) == 1 in
    Cancellation_context {purpose; protected}
  | 21 -> System_thread
  | _ -> assert false

(* Runtime events registration *)

type Runtime_events.User.tag += Created

let created_type =
  let encode buf ((child : int), (thread_type : event)) =
    Bytes.set_int32_le buf 0 (Int32.of_int child);
    4 + serialize_thread_type ~ofs:4 buf thread_type
  in
  let decode buf _size =
    let child = Bytes.get_int32_le buf 0 |> Int32.to_int in
    let thread_type = parse_thread_type ~ofs:4 buf in
    (child, thread_type)
  in
  Runtime_events.Type.register ~encode ~decode

let created = Runtime_events.User.register "eio.created" Created created_type

let two_ids_type =
  let encode buf ((child : int), i) =
    Bytes.set_int32_le buf 0 (Int32.of_int child);
    Bytes.set_int32_le buf 4 (Int32.of_int i);
    8
  in
  let decode buf _size =
    let child = Bytes.get_int32_le buf 0 |> Int32.to_int  in
    let i = Bytes.get_int32_le buf 4 |> Int32.to_int  in
    (child, i)
  in
  Runtime_events.Type.register ~encode ~decode

type Runtime_events.User.tag += Read
let read = Runtime_events.User.register "eio.read" Read two_ids_type

type Runtime_events.User.tag += Try_read
let try_read = Runtime_events.User.register "eio.try_read" Try_read two_ids_type

type Runtime_events.User.tag += Parent
let parent = Runtime_events.User.register "eio.parent" Parent two_ids_type

type Runtime_events.User.tag += Failed

let labelled_type =
  let encode buf ((child : int), exn) =
    (* Check size of buf and use smallest size which means we may
      have to truncate the label. *)
    let available_buf_len = Bytes.length buf - 1 in
    let exn_len = String.length exn in
    let data_len = min available_buf_len exn_len in
    Bytes.set_int32_le buf 0 (Int32.of_int child);
    Bytes.blit_string exn 0 buf 4 data_len;
    data_len + 4
  in
  let decode buf size =
    let child = Bytes.get_int32_le buf 0 |> Int32.to_int in
    let size = size - 4 in
    let target = Bytes.create size in
    Bytes.blit buf 4 target 0 size;
    (child, Bytes.unsafe_to_string target)
  in
  Runtime_events.Type.register ~encode ~decode
let failed = Runtime_events.User.register "eio.fail" Failed labelled_type
type Runtime_events.User.tag += Resolved
let resolved = Runtime_events.User.(register "eio.resolved" Resolved Runtime_events.Type.int)

type Runtime_events.User.tag += Name
let named = Runtime_events.User.register "eio.name" Name labelled_type

type Runtime_events.User.tag += Loc
let located = Runtime_events.User.register "eio.loc" Loc labelled_type
type Runtime_events.User.tag += Log
let logged = Runtime_events.User.register "eio.log" Log labelled_type

type Runtime_events.User.tag += Switch
let switch = Runtime_events.User.register "eio.switch" Switch Runtime_events.Type.int

type Runtime_events.User.tag += Signal
let signal = Runtime_events.User.register "eio.signal" Signal two_ids_type

type Runtime_events.User.tag += Suspend
let suspend = Runtime_events.User.register "eio.suspend" Suspend Runtime_events.Type.unit

(* Runtime events generation *)

let add_event = Runtime_events.User.write

let note_created child thread_type =
  assert ((child :> int) >= 0);
  add_event created (child, thread_type)

let note_read ~reader input =
  add_event read (reader, input)

let note_try_read thread input =
  add_event try_read (thread, input)

let note_signal ~src dst =
  add_event signal (src, dst)

let note_resolved p ~ex =
  match ex with
  | Some ex ->
      let msg = Printexc.to_string ex in
      add_event failed (p, msg)
  | None ->
      add_event resolved p

let note_log thread msg =
  add_event logged (thread, msg)

let note_location thread msg =
  add_event located (thread, msg)

let note_name thread msg =
  add_event named (thread, msg)

let note_switch new_current = add_event switch new_current

let note_hiatus Wait_for_work = add_event suspend ()

let note_parent ~child ~parent:p =
  add_event parent (child, p)
