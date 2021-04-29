type 'a t = (('a, exn) result -> unit) Queue.t

effect Await : Ctf.id * 'a t -> 'a

let create = Queue.create

let add_waiter t cb =
  Queue.add cb t

let wake_all t v =
  Queue.iter (fun f -> f v) t

let wake_one t v =
  match Queue.take_opt t with
  | None -> `Queue_empty
  | Some f -> f v; `Ok

let await t id =
  perform (Await (id, t))
