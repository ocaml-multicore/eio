type t = unit -> unit                (* A function to remove the hook *)

let null = ignore

let remove t = t ()
