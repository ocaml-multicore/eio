type id = int

type ty =
  | Fiber
  | Promise
  | Semaphore
  | Switch
  | Stream
  | Mutex

let ty_to_uint8 = function
  | Fiber -> 1
  | Promise -> 15
  | Semaphore -> 16
  | Switch -> 17
  | Stream -> 18
  | Mutex -> 19

let ty_of_uint8 = function
  | 1 -> Fiber
  | 15 -> Promise
  | 16 -> Semaphore
  | 17 -> Switch
  | 18 -> Stream
  | 19 -> Mutex
  | _ -> assert false

let ty_to_string (t : ty) =
  match t with
  | Fiber -> "fiber"
  | Promise -> "promise"
  | Semaphore -> "semaphore"
  | Switch -> "switch"
  | Stream -> "stream"
  | Mutex -> "mutex"
 
let string =
  let encode buf s =
    let len = min (Bytes.length buf) (String.length s) in
    Bytes.blit_string s 0 buf 0 len;
    len
  in
  let decode buf len = Bytes.sub_string buf 0 len in
  Runtime_events.Type.register ~encode ~decode

let id_ty_type =
  let encode buf (id, ty) =
    Bytes.set_int64_le buf 0 (Int64.of_int id);
    Bytes.set_int8 buf 8 (ty_to_uint8 ty);
    9
  in
  let decode buf _size =
    let id = Bytes.get_int64_le buf 0 |> Int64.to_int in
    let ty = ty_of_uint8 (Bytes.get_int8 buf 8) in
    (id, ty)
  in
  Runtime_events.Type.register ~encode ~decode

let id_string_type =
  let encode buf (id, msg) =
    (* Check size of buf and use smallest size which means we may
      have to truncate the label. *)
    let available_buf_len = Bytes.length buf - 8 in
    let msg_len = String.length msg in
    let data_len = min available_buf_len msg_len in
    Bytes.set_int64_le buf 0 (Int64.of_int id);
    Bytes.blit_string msg 0 buf 8 data_len;
    data_len + 8
  in
  let decode buf size =
    let id = Bytes.get_int64_le buf 0 |> Int64.to_int in
    (id, Bytes.sub_string buf 8 (size - 8))
  in
  Runtime_events.Type.register ~encode ~decode

let exn_type =
  let encode buf (id, exn) =
    (* Check size of buf and use smallest size which means we may
      have to truncate the label. *)
    let available_buf_len = Bytes.length buf - 8 in
    let msg = Printexc.to_string exn in
    let msg_len = String.length msg in
    let data_len = min available_buf_len msg_len in
    Bytes.set_int64_le buf 0 (Int64.of_int id);
    Bytes.blit_string msg 0 buf 8 data_len;
    data_len + 8
  in
  let decode buf size =
    let id = Bytes.get_int64_le buf 0 |> Int64.to_int in
    (id, Failure (Bytes.sub_string buf 8 (size - 8)))
  in
  Runtime_events.Type.register ~encode ~decode

(* Runtime events registration *)

type Runtime_events.User.tag += Create
let create = Runtime_events.User.register "eio.create" Create id_ty_type

type Runtime_events.User.tag += Read
let read = Runtime_events.User.register "eio.read" Read Runtime_events.Type.int

type Runtime_events.User.tag += Try_read
let try_read = Runtime_events.User.register "eio.try_read" Try_read Runtime_events.Type.int

type Runtime_events.User.tag += Resolve | Resolve_error
let resolve = Runtime_events.User.register "eio.resolve" Resolve Runtime_events.Type.int
let resolve_error = Runtime_events.User.register "eio.resolve_error" Resolve_error exn_type

type Runtime_events.User.tag += Name
let name = Runtime_events.User.register "eio.name" Name id_string_type

type Runtime_events.User.tag += Log
let log = Runtime_events.User.register "eio.log" Log string

type Runtime_events.User.tag += Fiber
let fiber = Runtime_events.User.register "eio.fiber" Fiber Runtime_events.Type.int

type Runtime_events.User.tag += Signal
let signal = Runtime_events.User.register "eio.signal" Signal Runtime_events.Type.int

type Runtime_events.User.tag += Suspend
let suspend = Runtime_events.User.register "eio.suspend" Suspend Runtime_events.Type.span

type 'a handler = int -> Runtime_events.Timestamp.t -> 'a -> unit

let ignore_event : _ handler = fun _ring_id _ts _data -> ()

let add_callbacks
    ?(create=ignore_event)
    ?(read=ignore_event)
    ?(try_read=ignore_event)
    ?(resolve=ignore_event)
    ?(resolve_error=ignore_event)
    ?(name=ignore_event)
    ?(log=ignore_event)
    ?(fiber=ignore_event)
    ?(signal=ignore_event)
    ?(suspend=ignore_event)
    x =
  let create_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Create -> create ring_id ts v
    | _ -> assert false
  in
  let int_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Read -> read ring_id ts v
    | Try_read -> try_read ring_id ts v
    | Resolve -> resolve ring_id ts v
    | Fiber -> fiber ring_id ts v
    | Signal -> signal ring_id ts v
    | _ -> ()
  in
  let span_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Suspend -> suspend ring_id ts v
    | _ -> ()
  in
  let int_exn_event ring_id ts ev (id, ex) =
    match Runtime_events.User.tag ev, ex with
    | Resolve_error, Failure msg -> resolve_error ring_id ts (id, msg)
    | _ -> ()
  in
  let id_string_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Name -> name ring_id ts v
    | _ -> ()
  in
  let string_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Log -> log ring_id ts v
    | _ -> ()
  in
  x
  |> Runtime_events.Callbacks.add_user_event id_ty_type create_event
  |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.int int_event
  |> Runtime_events.Callbacks.add_user_event exn_type int_exn_event
  |> Runtime_events.Callbacks.add_user_event string string_event
  |> Runtime_events.Callbacks.add_user_event id_string_type id_string_event
  |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.span span_event
