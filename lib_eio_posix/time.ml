open Eio.Std

module Mono_clock = struct
  type t = unit
  type time = Mtime.t

  let now () = Mtime_clock.now ()
  let sleep_until () time = Low_level.sleep_until time
end

let mono_clock : Mtime.t Eio.Time.clock_ty r =
  let handler = Eio.Time.Pi.clock (module Mono_clock) in
  Eio.Resource.T ((), handler)

module Clock = struct
  type t = unit
  type time = float

  let now () = Unix.gettimeofday ()

  let sleep_until () time =
    (* todo: use the realtime clock directly instead of converting to monotonic time.
       That is needed to handle adjustments to the system clock correctly. *)
    let d = time -. Unix.gettimeofday () in
    Eio.Time.Mono.sleep mono_clock d
end

let clock : float Eio.Time.clock_ty r =
  let handler = Eio.Time.Pi.clock (module Clock) in
  Eio.Resource.T ((), handler)
