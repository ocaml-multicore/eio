open Std

type ty = [ `Vars ]
type 'a t = ([> ty] as 'a) r

module Pi = struct
  module type VARS = sig
    type t

    val get_all : t -> (string * string) list
    val get : t -> string -> string
    val put : t -> name:string -> value:string -> unit
  end

  type (_, _, _) Resource.pi +=
    | Vars : ('t, (module VARS with type t = 't), [> ty ]) Resource.pi

  let vars (type t) (module X : VARS with type t = t) =
    Resource.handler [ H (Vars, (module X)) ]
end

let get_all t =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Vars)) in
  X.get_all t

let get t name =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Vars)) in
  try X.get t name
  with Not_found as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "getting environment variable %s" name

let put t ~name ~value =
  let (Resource.T (t, ops)) = t in
  let module X = (val (Resource.get ops Pi.Vars)) in
  X.put t ~name ~value

