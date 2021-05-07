type 'a t = (('a, exn) result -> unit) Queue.t

let create = Queue.create

let add_waiter t cb =
  Queue.add cb t

let wake_all t v =
  Queue.iter (fun f -> f v) t
