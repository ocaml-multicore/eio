module Key = struct
  type t = Optint.Int63.t
  let compare = Optint.Int63.compare
end

module Job = struct
  type t = {
    time : Mtime.t;
    thread : unit Suspended.t;
  }

  let compare a b = Mtime.compare a.time b.time
end

module Q = Psq.Make(Key)(Job)

type t = {
  mutable sleep_queue: Q.t;
  mutable next_id : Optint.Int63.t;
}

let create () = { sleep_queue = Q.empty; next_id = Optint.Int63.zero }

let add t time thread =
  let id = t.next_id in
  t.next_id <- Optint.Int63.succ t.next_id;
  let sleeper = { Job.time; thread } in
  t.sleep_queue <- Q.add id sleeper t.sleep_queue;
  id

let remove t id =
  t.sleep_queue <- Q.remove id t.sleep_queue

let pop t ~now =
  match Q.min t.sleep_queue with
  | Some (_, { Job.time; thread }) when time <= now ->
    Eio.Private.Fiber_context.clear_cancel_fn thread.fiber;
    t.sleep_queue <- Option.get (Q.rest t.sleep_queue);
    `Due thread
  | Some (_, { Job.time; _ }) -> `Wait_until time
  | None -> `Nothing
