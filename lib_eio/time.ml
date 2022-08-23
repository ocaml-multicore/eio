exception Timeout

class virtual ['a] clock = object
  method virtual now : 'a
  method virtual sleep_until : 'a -> unit
  method virtual add_seconds : 'a -> float -> 'a
  method virtual to_seconds : 'a -> float
end

let now (t: (_ #clock)) = t#now

let sleep_until (t : (_ #clock)) time = t#sleep_until time

let sleep t d = sleep_until t (t#add_seconds t#now d)

let to_seconds t time = t#to_seconds time
let with_timeout t d = Fiber.first (fun () -> sleep t d; Error `Timeout)
let with_timeout_exn t d = Fiber.first (fun () -> sleep t d; raise Timeout)

module Timeout = struct
  type 'a t =
    | Timeout of 'a clock * float
    | Unlimited

  let none = Unlimited
  let of_s clock time = Timeout ((clock :> 'a clock), time)

  let run t fn =
    match t with
    | Unlimited -> fn ()
    | Timeout (clock, d) ->
      Fiber.first (fun () -> sleep clock d; Error `Timeout) fn

  let run_exn t fn =
    match t with
    | Unlimited -> fn ()
    | Timeout (clock, d) ->
      Fiber.first (fun () -> sleep clock d; raise Timeout) fn

  let pp f = function
    | Unlimited -> Fmt.string f "(no timeout)"
    | Timeout (_clock, d) ->
      if d >= 0.001 && d < 0.1 then
        Fmt.pf f "%.2gms" (d *. 1000.)
      else if d < 120. then
        Fmt.pf f "%.2gs" d
      else
        Fmt.pf f "%.2gm" (d /. 60.)
end
