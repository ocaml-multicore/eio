exception Timeout

type t = int64   (* Nanoseconds representation *)
type secs = float (* Fractional seconds representation *)

class virtual clock = object
  method virtual now_ns : t
  method virtual sleep_until : t -> unit
end

let to_seconds t = (Int64.to_float t) /. 1e9
let of_seconds s = (s *. 1e9) |> Float.round |> Int64.of_float

let now (t : #clock) = to_seconds t#now_ns  

let now_ns (t: #clock) = t#now_ns

let sleep_until (t : #clock) time = t#sleep_until time

let sleep t d = sleep_until t (Int64.add t#now_ns d)

let with_timeout t d = Fiber.first (fun () -> sleep t d; Error `Timeout)
let with_timeout_exn t d = Fiber.first (fun () -> sleep t d; raise Timeout)
