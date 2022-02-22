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
