open Eio.Std

type 'a t = [
  | `Return of 'a
  | `Raise of exn
  | `Await of 'a Eio.Promise.or_exn
  | `Yield_then of 'a t
  | `Run of unit -> 'a
]

let rec run = function
  | `Return x -> x
  | `Raise ex -> raise ex
  | `Await p -> Promise.await_exn p
  | `Yield_then t -> Fiber.yield (); run t
  | `Run fn -> fn ()

let rec map f = function
  | `Return x -> `Return (f x)
  | `Raise ex -> `Raise ex
  | `Await p -> `Run (fun () -> f (Promise.await_exn p))
  | `Yield_then t -> `Yield_then (map f t)
  | `Run fn -> `Run (fun () -> f (fn ()))
