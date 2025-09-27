(** Mock socket option helpers *)

val pp_sockopt_value : 'a Eio.Net.Sockopt.t -> 'a -> string
(** [pp_sockopt_value opt v] formats socket option [opt] with value [v] as a string
    for trace output. Returns [unknown] for unrecognised options. *)

val setsockopt : string -> 'a Eio.Net.Sockopt.t -> 'a -> unit
(** [setsockopt label opt v] simulates setting socket option [opt] to value [v].
    Outputs a trace message using [label] to identify the socket. *)

val getsockopt : string -> 'a Eio.Net.Sockopt.t -> 'a
(** [getsockopt label opt] simulates getting the value of socket option [opt].
    Outputs a trace message using [label] to identify the socket.
    Returns default mock values for all standard options.

    Default values:
    - Boolean options: [false]
    - Integer options: [0]
    - Timeout options: [0.0]

    @raise Failure if the option is not recognized. *)
