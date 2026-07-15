open Eio.Std
open Types

type _ Effect.t +=
  | Await_readable : Unix.file_descr -> unit Effect.t
  | Await_writable : Unix.file_descr -> unit Effect.t
  | Get_monotonic_clock : Eio.Time.Mono.ty r Effect.t
  | Pipe : Switch.t -> (source_ty r * sink_ty r) Effect.t

let await_readable fd = Effect.perform (Await_readable fd)
let await_writable fd = Effect.perform (Await_writable fd)

let pipe sw =
  let r, w = Effect.perform (Pipe sw) in
  let r = (r : source_ty r :> [< source_ty] r) in
  let w = (w : sink_ty r :> [< sink_ty] r) in
  r, w

module Rcfd = Rcfd
module Fork_action = Fork_action
module Thread_pool = Thread_pool

external eio_readlinkat : Unix.file_descr -> string -> Cstruct.t -> int = "eio_unix_readlinkat"

let read_link_unix fd path =
  match fd with
  | None -> Unix.readlink path
  | Some fd ->
    let rec aux size =
      let buf = Cstruct.create_unsafe size in
      let len = eio_readlinkat fd path buf in
      if len < size then Cstruct.to_string ~len buf
      else aux (size * 4)
    in
    aux 1024

let read_link fd path = Fd.use_exn_opt "readlink" fd (fun fd -> read_link_unix fd path)

external eio_fchmodat : Unix.file_descr -> string -> int -> int -> unit = "eio_unix_fchmodat"

let chmod_unix fd path ~flags ~mode = eio_fchmodat fd path mode flags

let chmod fd path ~flags ~mode =
  Fd.use_exn "chmod" fd (fun fd -> chmod_unix ~flags ~mode fd path)

external eio_fchownat : Unix.file_descr -> string -> int64 -> int64 -> int -> unit = "eio_unix_fchownat"

let chown_unix ~flags ~uid ~gid fd path =
  eio_fchownat fd path uid gid flags

let chown ~flags ~uid ~gid fd path =
  Fd.use_exn "chown" fd (fun fd -> chown_unix ~uid ~gid ~flags fd path)

type sockaddr = [ `Tcp of Eio.Net.Ipaddr.v4v6 * int
                | `Udp of Eio.Net.Ipaddr.v4v6 * int ]

external eio_getaddrinfo : string -> string -> (sockaddr list, Eio.Net.Getaddrinfo_error.t) result
  = "eio_unix_getaddrinfo"

let getaddrinfo ~service node =
  match eio_getaddrinfo node service with
  | Ok x -> List.rev (x :> Eio.Net.Sockaddr.t list)
  | Error e -> raise @@ Eio.Net.err @@ Eio.Net.Address_lookup_failed e

module Sockopt = struct
  (* Keep in sync with [find_sockopt] C function. *)
  type t =
    (* bool *)
    | TCP_CORK
    | TCP_QUICKACK
    | IP_FREEBIND
    | IP_BIND_ADDRESS_NO_PORT
    (* int *)
    | TCP_KEEPIDLE
    | TCP_KEEPINTVL
    | TCP_KEEPCNT
    | TCP_USER_TIMEOUT
    | TCP_MAXSEG
    | TCP_DEFER_ACCEPT
    | TCP_SYNCNT
    | TCP_WINDOW_CLAMP
    | TCP_FASTOPEN
    | IP_LOCAL_PORT_RANGE
    | IP_MTU_DISCOVER
    | IP_TTL
    | IP_MTU
    (* int option *)
    | TCP_LINGER2
    (* string option *)
    | TCP_CONGESTION
end

external setsockopt_int : Unix.file_descr -> Sockopt.t -> int -> unit = "caml_eio_sockopt_int_set"
external getsockopt_int : Unix.file_descr -> Sockopt.t -> int = "caml_eio_sockopt_int_get"
external setsockopt_string : Unix.file_descr -> Sockopt.t -> string -> unit = "caml_eio_sockopt_string_set"
external getsockopt_string : Unix.file_descr -> Sockopt.t -> string = "caml_eio_sockopt_string_get"

let err_run_sock fn x =
  try fn x
  with
  | Unix.Unix_error (code, name, arg) -> raise (Err.v code name arg)
  | Not_found -> raise (Eio.Net.err Invalid_option)

let getsockopt_int fd opt = Fd.use_exn "getsockopt_int" fd (fun fd -> err_run_sock (getsockopt_int fd) opt)
let getsockopt_string fd opt = Fd.use_exn "getsockopt_string" fd (fun fd -> err_run_sock (getsockopt_string fd) opt)

let setsockopt_int fd opt v = Fd.use_exn "setsockopt_int" fd (fun fd -> err_run_sock (setsockopt_int fd opt) v)
let setsockopt_string fd opt v = Fd.use_exn "setsockopt_string" fd (fun fd -> err_run_sock (setsockopt_string fd opt) v)

let setsockopt : type a. Fd.t -> a Eio.Net.Sockopt.t -> a -> unit = fun fd opt v ->
  let open Eio.Net.Sockopt in
  let open Sockopt in
  match opt with
  | TCP_CORK ->
    setsockopt_int fd TCP_CORK (if v then 1 else 0)
  | TCP_KEEPIDLE ->
    if v < 0 then
      Fmt.invalid_arg "TCP_KEEPIDLE must be non-negative, got %d" v;
    setsockopt_int fd TCP_KEEPIDLE v
  | TCP_KEEPINTVL ->
    if v < 0 then
      Fmt.invalid_arg "TCP_KEEPINTVL must be non-negative, got %d" v;
    setsockopt_int fd TCP_KEEPINTVL v
  | TCP_KEEPCNT ->
    if v < 0 then
      Fmt.invalid_arg "TCP_KEEPCNT must be non-negative, got %d" v;
    setsockopt_int fd TCP_KEEPCNT v
  | TCP_USER_TIMEOUT ->
    if v < 0 then
      Fmt.invalid_arg "TCP_USER_TIMEOUT must be non-negative, got %d" v;
    setsockopt_int fd TCP_USER_TIMEOUT v
  | TCP_MAXSEG ->
    if v < 0 then
      Fmt.invalid_arg "TCP_MAXSEG must be non-negative, got %d" v;
    setsockopt_int fd TCP_MAXSEG v
  | TCP_LINGER2 ->
    let v = match v with
      | None -> 0
      | Some n when n < 0 ->
        Fmt.invalid_arg "TCP_LINGER2 must be non-negative, got %d" n
      | Some n -> n
    in
    setsockopt_int fd TCP_LINGER2 v
  | TCP_DEFER_ACCEPT ->
    if v < 0 then
      Fmt.invalid_arg "TCP_DEFER_ACCEPT must be non-negative, got %d" v;
    setsockopt_int fd TCP_DEFER_ACCEPT v
  | TCP_CONGESTION ->
    setsockopt_string fd TCP_CONGESTION v
  | TCP_SYNCNT ->
    if v < 1 || v > 255 then
      Fmt.invalid_arg "TCP_SYNCNT must be between 1 and 255, got %d" v;
    setsockopt_int fd TCP_SYNCNT v
  | TCP_WINDOW_CLAMP ->
    if v < 0 then
      Fmt.invalid_arg "TCP_WINDOW_CLAMP must be non-negative, got %d" v;
    setsockopt_int fd TCP_WINDOW_CLAMP v
  | TCP_QUICKACK ->
    setsockopt_int fd TCP_QUICKACK (if v then 1 else 0)
  | TCP_FASTOPEN ->
    if v < 0 then
      Fmt.invalid_arg "TCP_FASTOPEN queue length must be non-negative, got %d" v;
    setsockopt_int fd TCP_FASTOPEN v
  | IP_FREEBIND ->
    setsockopt_int fd IP_FREEBIND (if v then 1 else 0)
  | IP_BIND_ADDRESS_NO_PORT ->
    setsockopt_int fd IP_BIND_ADDRESS_NO_PORT (if v then 1 else 0)
  | IP_LOCAL_PORT_RANGE ->
    let (lower, upper) = v in
    if lower < 0 || lower > 65535 then
      Fmt.invalid_arg "IP_LOCAL_PORT_RANGE lower bound must be 0-65535, got %d" lower;
    if upper < 0 || upper > 65535 then
      Fmt.invalid_arg "IP_LOCAL_PORT_RANGE upper bound must be 0-65535, got %d" upper;
    if lower <> 0 && upper <> 0 && lower > upper then
      Fmt.invalid_arg "IP_LOCAL_PORT_RANGE lower bound (%d) must be <= upper bound (%d)" lower upper;
    let combined = (upper lsl 16) lor lower in
    setsockopt_int fd IP_LOCAL_PORT_RANGE combined
  | IP_TTL ->
    if v < 1 || v > 255 then
      Fmt.invalid_arg "IP_TTL must be between 1 and 255, got %d" v;
    setsockopt_int fd IP_TTL v
  | IP_MTU ->
    invalid_arg "IP_MTU is a read-only socket option"
  | IP_MTU_DISCOVER ->
    let i = match v with
      | `Dont -> 0
      | `Want -> 1
      | `Do -> 2
      | `Probe -> 3 in
    setsockopt_int fd IP_MTU_DISCOVER i
  | _ ->
    Fd.use_exn "setsockopt" fd @@ fun fd ->
    match opt with
    | Eio.Net.Sockopt.SO_DEBUG -> Unix.setsockopt fd Unix.SO_DEBUG v
    | Eio.Net.Sockopt.SO_BROADCAST -> Unix.setsockopt fd Unix.SO_BROADCAST v
    | Eio.Net.Sockopt.SO_REUSEADDR -> Unix.setsockopt fd Unix.SO_REUSEADDR v
    | Eio.Net.Sockopt.SO_KEEPALIVE -> Unix.setsockopt fd Unix.SO_KEEPALIVE v
    | Eio.Net.Sockopt.SO_DONTROUTE -> Unix.setsockopt fd Unix.SO_DONTROUTE v
    | Eio.Net.Sockopt.SO_OOBINLINE -> Unix.setsockopt fd Unix.SO_OOBINLINE v
    | Eio.Net.Sockopt.TCP_NODELAY -> Unix.setsockopt fd Unix.TCP_NODELAY v
    | Eio.Net.Sockopt.IPV6_ONLY -> Unix.setsockopt fd Unix.IPV6_ONLY v
    | Eio.Net.Sockopt.SO_REUSEPORT -> Unix.setsockopt fd Unix.SO_REUSEPORT v
    | Eio.Net.Sockopt.SO_SNDBUF -> Unix.setsockopt_int fd Unix.SO_SNDBUF v
    | Eio.Net.Sockopt.SO_RCVBUF -> Unix.setsockopt_int fd Unix.SO_RCVBUF v
    | Eio.Net.Sockopt.SO_RCVLOWAT -> Unix.setsockopt_int fd Unix.SO_RCVLOWAT v
    | Eio.Net.Sockopt.SO_SNDLOWAT -> Unix.setsockopt_int fd Unix.SO_SNDLOWAT v
    | Eio.Net.Sockopt.SO_LINGER -> Unix.setsockopt_optint fd Unix.SO_LINGER v
    | Eio.Net.Sockopt.SO_RCVTIMEO -> Unix.setsockopt_float fd Unix.SO_RCVTIMEO v
    | Eio.Net.Sockopt.SO_SNDTIMEO -> Unix.setsockopt_float fd Unix.SO_SNDTIMEO v
    | Net.Sockopt_bool bo -> Unix.setsockopt fd bo v
    | Net.Sockopt_int bo -> Unix.setsockopt_int fd bo v
    | Net.Sockopt_optint bo -> Unix.setsockopt_optint fd bo v
    | Net.Sockopt_float bo -> Unix.setsockopt_float fd bo v
    | _ -> raise (Eio.Net.err Invalid_option)

let getsockopt : type a. Fd.t -> a Eio.Net.Sockopt.t -> a = fun fd opt ->
  let open Sockopt in
  let open Eio.Net.Sockopt in
  match opt with
  | TCP_CORK ->
    getsockopt_int fd TCP_CORK <> 0
  | TCP_KEEPIDLE ->
    getsockopt_int fd TCP_KEEPIDLE
  | TCP_KEEPINTVL ->
    getsockopt_int fd TCP_KEEPINTVL
  | TCP_KEEPCNT ->
    getsockopt_int fd TCP_KEEPCNT
  | TCP_USER_TIMEOUT ->
    getsockopt_int fd TCP_USER_TIMEOUT
  | TCP_MAXSEG ->
    getsockopt_int fd TCP_MAXSEG
  | TCP_LINGER2 ->
    let v = getsockopt_int fd TCP_LINGER2 in
    if v < 0 then None else Some v
  | TCP_DEFER_ACCEPT ->
    getsockopt_int fd TCP_DEFER_ACCEPT
  | TCP_CONGESTION ->
    getsockopt_string fd TCP_CONGESTION
  | TCP_SYNCNT ->
    getsockopt_int fd TCP_SYNCNT
  | TCP_WINDOW_CLAMP ->
    getsockopt_int fd TCP_WINDOW_CLAMP
  | TCP_QUICKACK ->
    getsockopt_int fd TCP_QUICKACK <> 0
  | TCP_FASTOPEN ->
    getsockopt_int fd TCP_FASTOPEN
  | IP_FREEBIND ->
    getsockopt_int fd IP_FREEBIND <> 0
  | IP_BIND_ADDRESS_NO_PORT ->
    getsockopt_int fd IP_BIND_ADDRESS_NO_PORT <> 0
  | IP_LOCAL_PORT_RANGE ->
    let combined = getsockopt_int fd IP_LOCAL_PORT_RANGE in
    let lower = combined land 0xFFFF in
    let upper = (combined lsr 16) land 0xFFFF in
    (lower, upper)
  | IP_TTL ->
    getsockopt_int fd IP_TTL
  | IP_MTU ->
    getsockopt_int fd IP_MTU
  | IP_MTU_DISCOVER ->
    begin
      let i = getsockopt_int fd IP_MTU_DISCOVER in
      match i with
      | 0 (* IP_PMTUDISC_DONT *)  -> `Dont
      | 1 (* IP_PMTUDISC_WANT *)  -> `Want
      | 2 (* IP_PMTUDISC_DO *)    -> `Do
      | 3 (* IP_PMTUDISC_PROBE *) -> `Probe
      | i -> Fmt.failwith "Unknown IP_MTU_DISCOVER value: %d" i
    end
  | _ ->
    Fd.use_exn "getsockopt" fd @@ fun fd : a ->
    match opt with
    | Eio.Net.Sockopt.SO_DEBUG -> Unix.getsockopt fd Unix.SO_DEBUG
    | Eio.Net.Sockopt.SO_BROADCAST -> Unix.getsockopt fd Unix.SO_BROADCAST
    | Eio.Net.Sockopt.SO_REUSEADDR -> Unix.getsockopt fd Unix.SO_REUSEADDR
    | Eio.Net.Sockopt.SO_KEEPALIVE -> Unix.getsockopt fd Unix.SO_KEEPALIVE
    | Eio.Net.Sockopt.SO_DONTROUTE -> Unix.getsockopt fd Unix.SO_DONTROUTE
    | Eio.Net.Sockopt.SO_OOBINLINE -> Unix.getsockopt fd Unix.SO_OOBINLINE
    | Eio.Net.Sockopt.TCP_NODELAY -> Unix.getsockopt fd Unix.TCP_NODELAY
    | Eio.Net.Sockopt.IPV6_ONLY -> Unix.getsockopt fd Unix.IPV6_ONLY
    | Eio.Net.Sockopt.SO_REUSEPORT -> Unix.getsockopt fd Unix.SO_REUSEPORT
    | Eio.Net.Sockopt.SO_SNDBUF -> Unix.getsockopt_int fd Unix.SO_SNDBUF
    | Eio.Net.Sockopt.SO_RCVBUF -> Unix.getsockopt_int fd Unix.SO_RCVBUF
    | Eio.Net.Sockopt.SO_RCVLOWAT -> Unix.getsockopt_int fd Unix.SO_RCVLOWAT
    | Eio.Net.Sockopt.SO_SNDLOWAT -> Unix.getsockopt_int fd Unix.SO_SNDLOWAT
    | Eio.Net.Sockopt.SO_LINGER -> Unix.getsockopt_optint fd Unix.SO_LINGER
    | Eio.Net.Sockopt.SO_RCVTIMEO -> Unix.getsockopt_float fd Unix.SO_RCVTIMEO
    | Eio.Net.Sockopt.SO_SNDTIMEO -> Unix.getsockopt_float fd Unix.SO_SNDTIMEO
    | Net.Sockopt_bool bo -> Unix.getsockopt fd bo
    | Net.Sockopt_int bo -> Unix.getsockopt_int fd bo
    | Net.Sockopt_optint bo -> Unix.getsockopt_optint fd bo
    | Net.Sockopt_float bo -> Unix.getsockopt_float fd bo
    | _ -> raise (Eio.Net.err Invalid_option)
