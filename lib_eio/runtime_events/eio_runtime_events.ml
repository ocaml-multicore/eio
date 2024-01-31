type id = int

type obj_ty =
  | Promise
  | Semaphore
  | Stream
  | Mutex

let obj_ty_to_uint8 = function
  | Promise -> 15
  | Semaphore -> 16
  | Stream -> 18
  | Mutex -> 19

let obj_ty_of_uint8 = function
  | 15 -> Promise
  | 16 -> Semaphore
  | 18 -> Stream
  | 19 -> Mutex
  | _ -> assert false

let obj_ty_to_string (t : obj_ty) =
  match t with
  | Promise -> "promise"
  | Semaphore -> "semaphore"
  | Stream -> "stream"
  | Mutex -> "mutex"

type cc_ty =
  | Switch
  | Protect
  | Sub
  | Root
  | Any

let cc_ty_to_uint8 = function
  | Switch -> 3
  | Protect -> 4
  | Sub -> 5
  | Root -> 6
  | Any -> 7

let cc_ty_of_uint8 = function
  | 3 -> Switch
  | 4 -> Protect
  | 5 -> Sub
  | 6 -> Root
  | 7 -> Any
  | _ -> assert false

let cc_ty_to_string = function
  | Switch -> "switch"
  | Protect -> "protect"
  | Sub -> "sub"
  | Root -> "root"
  | Any -> "any"

let string =
  let encode buf s =
    let len = min (Bytes.length buf) (String.length s) in
    Bytes.blit_string s 0 buf 0 len;
    len
  in
  let decode buf len = Bytes.sub_string buf 0 len in
  Runtime_events.Type.register ~encode ~decode

let id_obj_type =
  let encode buf (id, ty) =
    Bytes.set_int64_le buf 0 (Int64.of_int id);
    Bytes.set_int8 buf 8 (obj_ty_to_uint8 ty);
    9
  in
  let decode buf _size =
    let id = Bytes.get_int64_le buf 0 |> Int64.to_int in
    let ty = obj_ty_of_uint8 (Bytes.get_int8 buf 8) in
    (id, ty)
  in
  Runtime_events.Type.register ~encode ~decode

let id_id_type =
  let encode buf (id1, id2) =
    Bytes.set_int64_le buf 0 (Int64.of_int id1);
    Bytes.set_int64_le buf 8 (Int64.of_int id2);
    16
  in
  let decode buf _size =
    let id1 = Bytes.get_int64_le buf 0 |> Int64.to_int in
    let id2 = Bytes.get_int64_le buf 8 |> Int64.to_int in
    (id1, id2)
  in
  Runtime_events.Type.register ~encode ~decode

let id_cc_type =
  let encode buf (id, ty) =
    Bytes.set_int64_le buf 0 (Int64.of_int id);
    Bytes.set_int8 buf 8 (cc_ty_to_uint8 ty);
    9
  in
  let decode buf _size =
    let id = Bytes.get_int64_le buf 0 |> Int64.to_int in
    let ty = cc_ty_of_uint8 (Bytes.get_int8 buf 8) in
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

type Runtime_events.User.tag +=
  | Create_obj
  | Create_fiber
  | Get
  | Create_cc
  | Try_get
  | Put
  | Error
  | Exit_cc
  | Exit_fiber
  | Name
  | Log
  | Enter_span
  | Exit_span
  | Suspend_fiber
  | Fiber
  | Suspend_domain
  | Domain_spawn

let create_obj = Runtime_events.User.register "eio.create_obj" Create_obj id_obj_type
let create_cc = Runtime_events.User.register "eio.create_cc" Create_cc id_cc_type
let create_fiber = Runtime_events.User.register "eio.create_fiber" Create_fiber id_id_type

let get = Runtime_events.User.register "eio.get" Get Runtime_events.Type.int
let try_get = Runtime_events.User.register "eio.try_get" Try_get Runtime_events.Type.int
let put = Runtime_events.User.register "eio.put" Put Runtime_events.Type.int

let exit_cc = Runtime_events.User.register "eio.exit_cc" Exit_cc Runtime_events.Type.unit
let exit_fiber = Runtime_events.User.register "eio.exit_fiber" Exit_fiber Runtime_events.Type.int
let error = Runtime_events.User.register "eio.error" Error exn_type

let name = Runtime_events.User.register "eio.name" Name id_string_type
let log = Runtime_events.User.register "eio.log" Log string
let enter_span = Runtime_events.User.register "eio.enter_span" Enter_span string
let exit_span = Runtime_events.User.register "eio.exit_span" Exit_span Runtime_events.Type.unit

let fiber = Runtime_events.User.register "eio.fiber" Fiber Runtime_events.Type.int
let suspend_fiber = Runtime_events.User.register "eio.suspend_fiber" Suspend_fiber string
let suspend_domain = Runtime_events.User.register "eio.suspend_domain" Suspend_domain Runtime_events.Type.span
let domain_spawn = Runtime_events.User.register "eio.domain_spawn" Domain_spawn Runtime_events.Type.int

type event = [
  | `Create of id * [
      | `Fiber_in of id
      | `Cc of cc_ty
      | `Obj of obj_ty
    ]
  | `Fiber of id
  | `Name of id * string
  | `Log of string
  | `Enter_span of string
  | `Exit_span
  | `Get of id
  | `Try_get of id
  | `Put of id
  | `Error of (id * string)
  | `Exit_cc
  | `Exit_fiber of id
  | `Suspend_domain of Runtime_events.Type.span
  | `Suspend_fiber of string
  | `Domain_spawn of id
]

let pf = Format.fprintf

let pp_event f (e : event) =
  match e with
  | `Create (id, `Fiber_in cc) -> pf f "create fiber %d in CC %d" id cc
  | `Create (id, `Cc ty) -> pf f "create %s CC %d" (cc_ty_to_string ty) id
  | `Create (id, `Obj ty) -> pf f "create %s %d" (obj_ty_to_string ty) id
  | `Fiber id -> pf f "fiber %d is now running" id
  | `Name (id, name) -> pf f "%d is named %S" id name
  | `Log msg -> pf f "log: %S" msg
  | `Enter_span op -> pf f "enter span %S" op
  | `Exit_span -> pf f "exit span"
  | `Get id -> pf f "get from %d" id
  | `Try_get id -> pf f "waiting to get from %d" id
  | `Put id -> pf f "put %d" id
  | `Error (id, msg) -> pf f "%d fails: %S" id msg
  | `Exit_cc -> pf f "CC finishes"
  | `Exit_fiber id -> pf f "fiber %d finishes" id
  | `Suspend_domain Begin -> pf f "domain suspend"
  | `Suspend_domain End -> pf f "domain resume"
  | `Suspend_fiber op -> pf f "fiber suspended: %s" op
  | `Domain_spawn parent -> pf f "domain spawned by fiber %d" parent

type 'a handler = int -> Runtime_events.Timestamp.t -> 'a -> unit

let add_callbacks (fn : event handler) x =
  let create_event ring_id ts ev (id, ty) =
    match Runtime_events.User.tag ev with
    | Create_obj -> fn ring_id ts (`Create (id, `Obj ty))
    | _ -> ()
  in
  let create_cc_event ring_id ts ev (id, ty) =
    match Runtime_events.User.tag ev with
    | Create_cc -> fn ring_id ts (`Create (id, `Cc ty))
    | _ -> ()
  in
  let int_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Get -> fn ring_id ts (`Get v)
    | Try_get -> fn ring_id ts (`Try_get v)
    | Put -> fn ring_id ts (`Put v)
    | Fiber -> fn ring_id ts (`Fiber v)
    | Exit_fiber -> fn ring_id ts (`Exit_fiber v)
    | Domain_spawn -> fn ring_id ts (`Domain_spawn v)
    | _ -> ()
  in
  let span_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Suspend_domain -> fn ring_id ts (`Suspend_domain v)
    | _ -> ()
  in
  let id_id_event ring_id ts ev (id1, id2) =
    match Runtime_events.User.tag ev with
    | Create_fiber -> fn ring_id ts (`Create (id1, `Fiber_in id2))
    | _ -> ()
  in
  let int_exn_event ring_id ts ev (id, ex) =
    match Runtime_events.User.tag ev, ex with
    | Error, Failure msg -> fn ring_id ts (`Error (id, msg))
    | _ -> ()
  in
  let id_string_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Name -> fn ring_id ts (`Name v)
    | _ -> ()
  in
  let string_event ring_id ts ev v =
    match Runtime_events.User.tag ev with
    | Log -> fn ring_id ts (`Log v)
    | Enter_span -> fn ring_id ts (`Enter_span v)
    | Suspend_fiber -> fn ring_id ts (`Suspend_fiber v)
    | _ -> ()
  in
  let unit_event ring_id ts ev () =
    match Runtime_events.User.tag ev with
    | Exit_cc -> fn ring_id ts `Exit_cc
    | Exit_span -> fn ring_id ts `Exit_span
    | _ -> ()
  in
  x
  |> Runtime_events.Callbacks.add_user_event id_obj_type create_event
  |> Runtime_events.Callbacks.add_user_event id_id_type id_id_event
  |> Runtime_events.Callbacks.add_user_event id_cc_type create_cc_event
  |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.int int_event
  |> Runtime_events.Callbacks.add_user_event exn_type int_exn_event
  |> Runtime_events.Callbacks.add_user_event string string_event
  |> Runtime_events.Callbacks.add_user_event id_string_type id_string_event
  |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.span span_event
  |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.unit unit_event
