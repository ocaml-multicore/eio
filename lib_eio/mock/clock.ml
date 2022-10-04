open Eio.Std

module type S = sig
  type time

  type t = <
    time Eio.Time.clock_base;
    advance : unit;
    set_time : time -> unit;
  >

  val make : unit -> t
  val advance : t -> unit
  val set_time : t -> time -> unit
end

module type TIME = sig
  type t
  val zero : t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Make(T : TIME) : S with type time := T.t = struct
  type t = <
    T.t Eio.Time.clock_base;
    advance : unit;
    set_time : T.t -> unit;
  >

  module Key = struct
    type t = < >
    let compare = compare
  end

  module Job = struct
    type t = {
      time : T.t;
      resolver : unit Promise.u;
    }

    let compare a b = T.compare a.time b.time
  end

  module Q = Psq.Make(Key)(Job)

  let make () =
    object (self)
      inherit [T.t] Eio.Time.clock_base

      val mutable now = T.zero
      val mutable q = Q.empty

      method now = now

      method sleep_until time =
        if T.compare time now <= 0 then Fiber.yield ()
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
          | Some (_, v) when T.compare v.time time <= 0 ->
            Promise.resolve v.resolver ();
            q <- Option.get (Q.rest q);
            drain ()
          | _ -> ()
        in
        drain ();
        now <- time;
        traceln "mock time is now %a" T.pp now

      method advance =
        match Q.min q with
        | None -> invalid_arg "No further events scheduled on mock clock"
        | Some (_, v) -> self#set_time v.time
    end

    let set_time (t:t) time = t#set_time time
    let advance (t:t) = t#advance
end

module Old_time = struct
  type t = float
  let compare = Float.compare
  let pp f x = Fmt.pf f "%g" x
  let zero = 0.0
end

module Mono_time = struct
  type t = Mtime.t
  let compare = Mtime.compare
  let zero = Mtime.of_uint64_ns 0L

  let pp f t =
    let s = Int64.to_float (Mtime.to_uint64_ns t) /. 1e9 in
    Fmt.pf f "%g" s
end

module Mono = Make(Mono_time)

include Make(Old_time)
