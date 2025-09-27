open Eio.Std

let pp_sockopt_value : type a. a Eio.Net.Sockopt.t -> a -> string = fun opt v ->
  match opt with
  | Eio.Net.Sockopt.SO_DEBUG -> Fmt.str "SO_DEBUG = %b" v
  | Eio.Net.Sockopt.SO_BROADCAST -> Fmt.str "SO_BROADCAST = %b" v
  | Eio.Net.Sockopt.SO_REUSEADDR -> Fmt.str "SO_REUSEADDR = %b" v
  | Eio.Net.Sockopt.SO_KEEPALIVE -> Fmt.str "SO_KEEPALIVE = %b" v
  | Eio.Net.Sockopt.SO_DONTROUTE -> Fmt.str "SO_DONTROUTE = %b" v
  | Eio.Net.Sockopt.SO_OOBINLINE -> Fmt.str "SO_OOBINLINE = %b" v
  | Eio.Net.Sockopt.TCP_NODELAY -> Fmt.str "TCP_NODELAY = %b" v
  | Eio.Net.Sockopt.IPV6_ONLY -> Fmt.str "IPV6_ONLY = %b" v
  | Eio.Net.Sockopt.SO_REUSEPORT -> Fmt.str "SO_REUSEPORT = %b" v
  | Eio.Net.Sockopt.SO_SNDBUF -> Fmt.str "SO_SNDBUF = %d" v
  | Eio.Net.Sockopt.SO_RCVBUF -> Fmt.str "SO_RCVBUF = %d" v
  | Eio.Net.Sockopt.SO_TYPE -> Fmt.str "SO_TYPE = %d" v
  | Eio.Net.Sockopt.SO_RCVLOWAT -> Fmt.str "SO_RCVLOWAT = %d" v
  | Eio.Net.Sockopt.SO_SNDLOWAT -> Fmt.str "SO_SNDLOWAT = %d" v
  | Eio.Net.Sockopt.SO_LINGER -> Fmt.str "SO_LINGER = %s" (match v with None -> "None" | Some n -> string_of_int n)
  | Eio.Net.Sockopt.SO_RCVTIMEO -> Fmt.str "SO_RCVTIMEO = %f" v
  | Eio.Net.Sockopt.SO_SNDTIMEO -> Fmt.str "SO_SNDTIMEO = %f" v
  | _ -> "unknown"

let setsockopt : type a. string -> a Eio.Net.Sockopt.t -> a -> unit = fun label opt v ->
  let opt_desc = pp_sockopt_value opt v in
  traceln "%s: setsockopt %s" label opt_desc

let getsockopt : type a. string -> a Eio.Net.Sockopt.t -> a = fun label opt ->
  match opt with
  | Eio.Net.Sockopt.SO_DEBUG -> traceln "%s: getsockopt SO_DEBUG = false" label; false
  | Eio.Net.Sockopt.SO_BROADCAST -> traceln "%s: getsockopt SO_BROADCAST = false" label; false
  | Eio.Net.Sockopt.SO_REUSEADDR -> traceln "%s: getsockopt SO_REUSEADDR = false" label; false
  | Eio.Net.Sockopt.SO_KEEPALIVE -> traceln "%s: getsockopt SO_KEEPALIVE = false" label; false
  | Eio.Net.Sockopt.SO_DONTROUTE -> traceln "%s: getsockopt SO_DONTROUTE = false" label; false
  | Eio.Net.Sockopt.SO_OOBINLINE -> traceln "%s: getsockopt SO_OOBINLINE = false" label; false
  | Eio.Net.Sockopt.TCP_NODELAY -> traceln "%s: getsockopt TCP_NODELAY = false" label; false
  | Eio.Net.Sockopt.IPV6_ONLY -> traceln "%s: getsockopt IPV6_ONLY = false" label; false
  | Eio.Net.Sockopt.SO_REUSEPORT -> traceln "%s: getsockopt SO_REUSEPORT = false" label; false
  | Eio.Net.Sockopt.SO_SNDBUF -> traceln "%s: getsockopt SO_SNDBUF = 0" label; 0
  | Eio.Net.Sockopt.SO_RCVBUF -> traceln "%s: getsockopt SO_RCVBUF = 0" label; 0
  | Eio.Net.Sockopt.SO_TYPE -> traceln "%s: getsockopt SO_TYPE = 0" label; 0
  | Eio.Net.Sockopt.SO_RCVLOWAT -> traceln "%s: getsockopt SO_RCVLOWAT = 0" label; 0
  | Eio.Net.Sockopt.SO_SNDLOWAT -> traceln "%s: getsockopt SO_SNDLOWAT = 0" label; 0
  | Eio.Net.Sockopt.SO_LINGER -> traceln "%s: getsockopt SO_LINGER = None" label; None
  | Eio.Net.Sockopt.SO_RCVTIMEO -> traceln "%s: getsockopt SO_RCVTIMEO = 0.0" label; 0.0
  | Eio.Net.Sockopt.SO_SNDTIMEO -> traceln "%s: getsockopt SO_SNDTIMEO = 0.0" label; 0.0
  | _ -> raise (Failure "Mock getsockopt not implemented")