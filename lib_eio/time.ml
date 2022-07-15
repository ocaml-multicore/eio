exception Timeout

(* TODO make sure the args are correct, i.e. can't have negative value as time. 
   when creating t. of_seconds, of_nanoseconds sleep must guard against negative 
   values.
*)

type t = int64   (* Nanoseconds representation *)

class virtual clock = object
  method virtual now : t
  method virtual sleep_until : t -> unit
end

external to_nanoseconds : t -> int64 = "%identity"
external of_nanoseconds : int64 -> t = "%identity"

let add t t2 = Int64.add t t2
let sub t t2 = Int64.sub t t2
let to_seconds t = (Int64.to_float t) /. 1e9
let of_seconds s = (s *. 1e9) |> Float.round |> Int64.of_float
let to_string = Int64.to_string
let pp fmt t = Format.fprintf fmt "%Ld" t
let compare = Int64.compare

let now (t: #clock) = t#now

let sleep_until (t : #clock) time = t#sleep_until time

let sleep t duration = sleep_until t (Int64.add t#now duration)

let with_timeout t duration = Fiber.first (fun () -> sleep t duration; Error `Timeout)
let with_timeout_exn t duration = Fiber.first (fun () -> sleep t duration; raise Timeout)
