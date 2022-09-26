open Eio.Std

type t = <
  Eio.Time.clock;
  advance : unit;
  set_time : float -> unit;
>

module Key = struct
  type t = < >
  let compare = compare
end

module Job = struct
  type t = {
    time : float;
    resolver : unit Promise.u;
  }

  let compare a b = Float.compare a.time b.time
end

module Q = Psq.Make(Key)(Job)

let make () =
  object (self)
    inherit Eio.Time.clock

    val mutable now = 0.0
    val mutable q = Q.empty

    method now = now

    method sleep_until time =
      if time <= now then Fiber.yield ()
      else (
        let p, r = Promise.create () in
        let k = object end in
        q <- Q.add k { time; resolver = r } q;
        try
          Promise.await p
        with Eio.Cancel.Cancelled _ as ex ->
          q <- Q.remove k q;
          raise ex
      )

    method set_time time =
      let rec drain () =
        match Q.min q with
        | Some (_, v) when v.time <= time ->
          Promise.resolve v.resolver ();
          q <- Option.get (Q.rest q);
          drain ()
        | _ -> ()
      in
      drain ();
      now <- time;
      traceln "mock time is now %g" now

    method advance =
      match Q.min q with
      | None -> invalid_arg "No further events scheduled on mock clock"
      | Some (_, v) -> self#set_time v.time
  end

let set_time (t:t) time = t#set_time time
let advance (t:t) = t#advance
