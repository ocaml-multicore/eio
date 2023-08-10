open Std

module Unix_perm = struct
  type t = int
end

module Stat = struct
  type kind = [
    | `Unknown
    | `Fifo
    | `Character_special
    | `Directory
    | `Block_device
    | `Regular_file
    | `Symbolic_link
    | `Socket
  ]

  type t = {
    dev : Int64.t;
    ino : Int64.t;
    kind : kind;
    perm : Unix_perm.t;
    nlink : Int64.t;
    uid : Int64.t;
    gid : Int64.t;
    rdev : Int64.t;
    size : Optint.Int63.t;
    atime : float;
    mtime : float;
    ctime : float;
  }
end

type ro_ty = [`File | Flow.source_ty | Resource.close_ty]

type 'a ro = ([> ro_ty] as 'a) r

type rw_ty = [ro_ty | Flow.sink_ty]

type 'a rw = ([> rw_ty] as 'a) r

module Pi = struct
  module type READ = sig
    include Flow.Pi.SOURCE

    val pread : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
    val stat : t -> Stat.t
    val close : t -> unit
  end

  module type WRITE = sig
    include Flow.Pi.SINK
    include READ with type t := t

    val pwrite : t -> file_offset:Optint.Int63.t -> Cstruct.t list -> int
  end

  type (_, _, _) Resource.pi +=
    | Read : ('t, (module READ with type t = 't), [> ro_ty]) Resource.pi
    | Write : ('t, (module WRITE with type t = 't), [> rw_ty]) Resource.pi

  let ro (type t) (module X : READ with type t = t) =
    Resource.handler [
      H (Flow.Pi.Source, (module X));
      H (Read, (module X));
      H (Resource.Close, X.close);
    ]

  let rw (type t) (module X : WRITE with type t = t) =
    Resource.handler (
      H (Flow.Pi.Sink, (module X)) ::
      H (Write, (module X)) ::
      Resource.bindings (ro (module X))
    )
end

let stat (Resource.T (t, ops)) =
  let module X = (val (Resource.get ops Pi.Read)) in
  X.stat t

let size t = (stat t).size

let pread (Resource.T (t, ops)) ~file_offset bufs =
  let module X = (val (Resource.get ops Pi.Read)) in
  let got = X.pread t ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let pread_exact (Resource.T (t, ops)) ~file_offset bufs =
  let module X = (val (Resource.get ops Pi.Read)) in
  let rec aux ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then (
      let got = X.pread t ~file_offset bufs in
      let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
      aux ~file_offset (Cstruct.shiftv bufs got)
    )
  in
  aux ~file_offset bufs

let pwrite_single (Resource.T (t, ops)) ~file_offset bufs =
  let module X = (val (Resource.get ops Pi.Write)) in
  let got = X.pwrite t ~file_offset bufs in
  assert (got > 0 && got <= Cstruct.lenv bufs);
  got

let pwrite_all (Resource.T (t, ops)) ~file_offset bufs =
  let module X = (val (Resource.get ops Pi.Write)) in
  let rec aux ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then (
      let got = X.pwrite t ~file_offset bufs in
      let file_offset = Optint.Int63.add file_offset (Optint.Int63.of_int got) in
      aux ~file_offset (Cstruct.shiftv bufs got)
    )
  in
  aux ~file_offset bufs
