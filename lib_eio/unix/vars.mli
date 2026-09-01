include Eio.Vars.Pi.VARS with type t = Eio.Mutex.t

val get_path : sep:char -> t -> string list
(** [get_path ~sep t] gets the paths stored at ["PATH"] using [sep] to separate
    them. *)

val put_path : sep:string -> t -> string list -> unit
(** [put_path ~sep t paths] sets the paths at ["PATH"] using [sep] to combine
    them. *)
