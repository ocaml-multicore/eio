module Key = struct
  type t = Optint.Int63.t
  let compare = Optint.Int63.compare
end

module type SUSPENDED = sig
  type 'a t

  val fiber : 'a t -> Eio.Private.Fiber_context.t
end

module Time = Eio.Time

module Make (Suspended : SUSPENDED) = struct

  module Job = struct
    type t = {
      time : Time.t;
      thread : unit Suspended.t;
    }

    let compare a b = Time.compare a.time b.time
  end

  module Q = Psq.Make(Key)(Job)

  type t = {
    clock : Time.clock;
    mutable sleep_queue: Q.t;
    mutable next_id : Optint.Int63.t;
  }

  let create clock = { clock; sleep_queue = Q.empty; next_id = Optint.Int63.zero }

  let add t time thread =
    let id = t.next_id in
    t.next_id <- Optint.Int63.succ t.next_id;
    let sleeper = {Job.time; thread} in
    t.sleep_queue <- Q.add id sleeper t.sleep_queue;
    id

  let remove t id =
    t.sleep_queue <- Q.remove id t.sleep_queue

  let pop t =
    let now = Time.now t.clock in
    match Q.min t.sleep_queue with
    | Some (_, {Job.time; thread}) when (Time.compare now time = 1) ->
      if Eio.Private.Fiber_context.clear_cancel_fn (Suspended.fiber thread) then (
        t.sleep_queue <- Option.get (Q.rest t.sleep_queue);
        `Due thread
      ) else (
        (* This shouldn't happen, since any cancellation will happen in the same domain as the [pop]. *)
        assert false
      )
    | Some (_, {Job.time;_}) ->
      let time = Time.sub time now in
      `Wait_until time
    | None -> `Nothing
end
