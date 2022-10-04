exception Timeout

class virtual ['a] clock_base = object
  method virtual now : 'a
  method virtual sleep_until : 'a -> unit
end

class virtual clock = object
  inherit [float] clock_base
end

let now (t : _ #clock_base) = t#now

let sleep_until (t : _ #clock_base) time = t#sleep_until time

let sleep t d = sleep_until t (now t +. d)

module Mono = struct
  class virtual t = object
    inherit [Mtime.t] clock_base
  end

  let now = now
  let sleep_until = sleep_until

  let sleep_span t span =
    match Mtime.add_span (now t) span with
    | Some time -> sleep_until t time
    | None -> Fiber.await_cancel ()

  (* Converting floats via int64 is tricky when things overflow or go negative.
     Since we don't need to wait for more than 100 years, limit it to this: *)
  let too_many_ns = 0x8000000000000000.

  let span_of_s s =
    if s >= 0.0 then (
      let ns = s *. 1e9 in
      if ns >= too_many_ns then Mtime.Span.max_span
      else Mtime.Span.of_uint64_ns (Int64.of_float ns)
    ) else Mtime.Span.zero      (* Also happens for NaN and negative infinity *)

  let sleep (t : #t) s =
    sleep_span t (span_of_s s)
end

let with_timeout t d = Fiber.first (fun () -> sleep t d; Error `Timeout)
let with_timeout_exn t d = Fiber.first (fun () -> sleep t d; raise Timeout)

module Timeout = struct
  type t =
    | Timeout of Mono.t * Mtime.Span.t
    | Deprecated of clock * float
    | Unlimited

  let none = Unlimited
  let v clock time = Timeout ((clock :> Mono.t), time)

  let seconds clock time =
    v clock (Mono.span_of_s time)

  let of_s clock time =
    Deprecated ((clock :> clock), time)

  let run t fn =
    match t with
    | Unlimited -> fn ()
    | Timeout (clock, d) ->
      Fiber.first (fun () -> Mono.sleep_span clock d; Error `Timeout) fn
    | Deprecated (clock, d) ->
      Fiber.first (fun () -> sleep clock d; Error `Timeout) fn

  let run_exn t fn =
    match t with
    | Unlimited -> fn ()
    | Timeout (clock, d) ->
      Fiber.first (fun () -> Mono.sleep_span clock d; raise Timeout) fn
    | Deprecated (clock, d) ->
      Fiber.first (fun () -> sleep clock d; raise Timeout) fn

  let pp_duration f d =
    if d >= 0.001 && d < 0.1 then
      Fmt.pf f "%.2gms" (d *. 1000.)
    else if d < 120. then
      Fmt.pf f "%.2gs" d
    else
      Fmt.pf f "%.2gm" (d /. 60.)

  let pp f = function
    | Unlimited -> Fmt.string f "(no timeout)"
    | Timeout (_clock, d) ->
      let d = Mtime.Span.to_s d in
      pp_duration f d
    | Deprecated (_clock, d) ->
      pp_duration f d
end
