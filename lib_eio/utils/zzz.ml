module Key = struct
  type t = Optint.Int63.t
  let compare = Optint.Int63.compare
end

type item =
  | Fiber of unit Suspended.t
  | Fn of (unit -> unit)

module Job = struct
  type t = {
    time : Mtime.t;
    item : item;
  }

  let compare a b = Mtime.compare a.time b.time
end

module Q = Psq.Make(Key)(Job)

type t = {
  mutable sleep_queue: Q.t;
  mutable next_id : Optint.Int63.t;
}

let create () = { sleep_queue = Q.empty; next_id = Optint.Int63.zero }

let add t time item =
  let id = t.next_id in
  t.next_id <- Optint.Int63.succ t.next_id;
  let sleeper = { Job.time; item } in
  t.sleep_queue <- Q.add id sleeper t.sleep_queue;
  id

let remove t id =
  t.sleep_queue <- Q.remove id t.sleep_queue

let pop t ~now =
  match Q.min t.sleep_queue with
  | Some (_, { Job.time; item }) when time <= now ->
    begin
      match item with
      | Fiber k -> Eio.Private.Fiber_context.clear_cancel_fn k.fiber
      | Fn _ -> ()
    end;
    t.sleep_queue <- Option.get (Q.rest t.sleep_queue);
    `Due item
  | Some (_, { Job.time; _ }) -> `Wait_until time
  | None -> `Nothing
