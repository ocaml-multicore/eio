open Eio.Std

type 'time ty = [`Mock | 'time Eio.Time.clock_ty]

module type S = sig
  type time

  type t = time ty r

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
  type t = T.t ty r

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

  module Impl = struct
    type time = T.t

    type t = {
      mutable now : T.t;
      mutable q : Q.t;
    }

    let make () =
      {
        now = T.zero;
        q = Q.empty;
      }

    let now t = t.now

    let sleep_until t time =
      if T.compare time t.now <= 0 then Fiber.yield ()
      else (
        let p, r = Promise.create () in
        let k = object end in
        t.q <- Q.add k { time; resolver = r } t.q;
        try
          Promise.await p
        with Eio.Cancel.Cancelled _ as ex ->
          t.q <- Q.remove k t.q;
          raise ex
      )

    let set_time t time =
      let rec drain () =
        match Q.min t.q with
        | Some (_, v) when T.compare v.time time <= 0 ->
          Promise.resolve v.resolver ();
          t.q <- Option.get (Q.rest t.q);
          drain ()
        | _ -> ()
      in
      drain ();
      t.now <- time;
      traceln "mock time is now %a" T.pp t.now

    let advance t =
      match Q.min t.q with
      | None -> invalid_arg "No further events scheduled on mock clock"
      | Some (_, v) -> set_time t v.time

    type (_, _, _) Eio.Resource.pi += Raw : ('t, 't -> t, T.t ty) Eio.Resource.pi
    let raw (Eio.Resource.T (t, ops)) = Eio.Resource.get ops Raw t
  end

  let handler =
    Eio.Resource.handler (
      H (Impl.Raw, Fun.id) ::
      Eio.Resource.bindings (Eio.Time.Pi.clock (module Impl));
    )

  let make () =
    Eio.Resource.T (Impl.make (), handler)

  let set_time t v = Impl.set_time (Impl.raw t) v
  let advance t = Impl.advance (Impl.raw t)
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
