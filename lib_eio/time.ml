exception Timeout

class virtual clock = object
  method virtual now : float
  method virtual sleep_until : float -> unit
end

let now (t : #clock) = t#now

let sleep_until (t : #clock) time = t#sleep_until time

let sleep t d = sleep_until t (now t +. d)

let with_timeout t d = Fiber.first (fun () -> sleep t d; Error `Timeout)
let with_timeout_exn t d = Fiber.first (fun () -> sleep t d; raise Timeout)

module Timeout = struct
  type t =
    | Timeout of clock * float
    | Unlimited

  let none = Unlimited
  let of_s clock time = Timeout ((clock :> clock), time)

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
