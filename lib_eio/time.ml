exception Timeout

class clock = Eio_clock.t

let now (t : #clock) = t#now
let now_ns (t: #clock) = t#now_ns

let sleep_until (t : #clock) time = t#sleep_until time

let sleep t d = sleep_until t (now t +. d)

let with_timeout t d = Fiber.first (fun () -> sleep t d; Error `Timeout)
let with_timeout_exn t d = Fiber.first (fun () -> sleep t d; raise Timeout)
