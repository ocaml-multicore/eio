open Eio.Std

let setsockopt : type a. string -> a Eio.Net.Sockopt.t -> a -> unit = fun label opt v ->
  traceln "%s: setsockopt %a" label (Eio.Net.Sockopt.pp opt) v

let default (type a) (opt : a Eio.Net.Sockopt.t) : a =
  let open Eio.Net.Sockopt in
  match opt with
  | SO_DEBUG -> false
  | SO_BROADCAST -> false
  | SO_REUSEADDR -> false
  | SO_KEEPALIVE -> false
  | SO_DONTROUTE -> false
  | SO_OOBINLINE -> false
  | TCP_NODELAY -> false
  | IPV6_ONLY -> false
  | SO_REUSEPORT -> false
  | SO_SNDBUF -> 0
  | SO_RCVBUF -> 0
  | SO_TYPE -> 0
  | SO_RCVLOWAT -> 0
  | SO_SNDLOWAT -> 0
  | SO_LINGER -> None
  | SO_RCVTIMEO -> 0.0
  | SO_SNDTIMEO -> 0.0
  | _ -> raise (Failure "Mock getsockopt not implemented")

let getsockopt : type a. string -> a Eio.Net.Sockopt.t -> a = fun label opt ->
  let v = default opt in
  traceln "%s: getsockopt %a" label (Eio.Net.Sockopt.pp opt) v;
  v
