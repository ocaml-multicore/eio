open Std

type shutdown_command = [ `Receive | `Send | `All ]

type 't read_method = ..
type 't read_method += Read_source_buffer of ('t -> (Cstruct.t list -> int) -> unit)

type source_ty = [`R | `Flow]
type 'a source = ([> source_ty] as 'a) r

type sink_ty = [`W | `Flow]
type 'a sink = ([> sink_ty] as 'a) r

type shutdown_ty = [`Shutdown]
type 'a shutdown = ([> shutdown_ty] as 'a) r

module Pi = struct
  module type SOURCE = sig
    type t
    val read_methods : t read_method list
    val single_read : t -> Cstruct.t -> int
  end

  module type SINK = sig
    type t
    val single_write : t -> Cstruct.t list -> int
    val copy : t -> src:_ source -> unit
  end

  module type SHUTDOWN = sig
    type t
    val shutdown : t -> shutdown_command -> unit
  end

  type (_, _, _) Resource.pi +=
    | Source : ('t, (module SOURCE with type t = 't), [> source_ty]) Resource.pi
    | Sink : ('t, (module SINK with type t = 't), [> sink_ty]) Resource.pi
    | Shutdown : ('t, (module SHUTDOWN with type t = 't), [> shutdown_ty]) Resource.pi

  let source (type t) (module X : SOURCE with type t = t) =
    Resource.handler [H (Source, (module X))]

  let sink (type t) (module X : SINK with type t = t) =
    Resource.handler [H (Sink, (module X))]

  let shutdown (type t) (module X : SHUTDOWN with type t = t) =
    Resource.handler [ H (Shutdown, (module X))]

  module type TWO_WAY = sig
    include SHUTDOWN
    include SOURCE with type t := t
    include SINK with type t := t
  end

  let two_way (type t) (module X : TWO_WAY with type t = t) =
    Resource.handler [
      H (Shutdown, (module X));
      H (Source, (module X));
      H (Sink, (module X));
    ]

  let simple_copy ~single_write t ~src:(Resource.T (src, src_ops)) =
    let rec write_all buf =
      if not (Cstruct.is_empty buf) then (
        let sent = single_write t [buf] in
        write_all (Cstruct.shift buf sent)
      )
    in
    let module Src = (val (Resource.get src_ops Source)) in
    try
      let buf = Cstruct.create 4096 in
      while true do
        let got = Src.single_read src buf in
        write_all (Cstruct.sub buf 0 got)
      done
    with End_of_file -> ()
end

open Pi

let close = Resource.close

let single_read (Resource.T (t, ops)) buf =
  let module X = (val (Resource.get ops Source)) in
  let got = X.single_read t buf in
  assert (got > 0 && got <= Cstruct.length buf);
  got

let rec read_exact t buf =
  if Cstruct.length buf > 0 then (
    let got = single_read t buf in
    read_exact t (Cstruct.shift buf got)
  )

module Cstruct_source = struct
  type t = Cstruct.t list ref

  let create data = ref data

  let read_source_buffer t fn =
    let rec aux () =
      match !t with
      | [] -> raise End_of_file
      | x :: xs when Cstruct.length x = 0 -> t := xs; aux ()
      | xs ->
        let n = fn xs in
        t := Cstruct.shiftv xs n
    in
    aux ()

  let read_methods =
    [ Read_source_buffer read_source_buffer ]

  let single_read t dst =
    let avail, src = Cstruct.fillv ~dst ~src:!t in
    if avail = 0 then raise End_of_file;
    t := src;
    avail

end

let cstruct_source =
  let ops = Pi.source (module Cstruct_source) in
  fun data -> Resource.T (Cstruct_source.create data, ops)

module String_source = struct
  type t = {
    s : string;
    mutable offset : int;
  }

  let single_read t dst =
    if t.offset = String.length t.s then raise End_of_file;
    let len = min (Cstruct.length dst) (String.length t.s - t.offset) in
    Cstruct.blit_from_string t.s t.offset dst 0 len;
    t.offset <- t.offset + len;
    len

  let read_methods = []

  let create s = { s; offset = 0 }
end

let string_source =
  let ops = Pi.source (module String_source) in
  fun s -> Resource.T (String_source.create s, ops)

let single_write (Resource.T (t, ops)) bufs =
  let module X = (val (Resource.get ops Sink)) in
  X.single_write t bufs

let write (Resource.T (t, ops)) bufs =
  let module X = (val (Resource.get ops Sink)) in
  let rec aux = function
    | [] -> ()
    | bufs ->
      let wrote = X.single_write t bufs in
      aux (Cstruct.shiftv bufs wrote)
  in
  aux bufs

let copy src (Resource.T (t, ops)) =
  let module X = (val (Resource.get ops Sink)) in
  X.copy t ~src

let copy_string s = copy (string_source s)

module Buffer_sink = struct
  type t = Buffer.t

  let single_write t bufs =
    let old_length = Buffer.length t in
    List.iter (fun buf -> Buffer.add_bytes t (Cstruct.to_bytes buf)) bufs;
    Buffer.length t - old_length

  let copy t ~src = Pi.simple_copy ~single_write t ~src
end

let buffer_sink =
  let ops = Pi.sink (module Buffer_sink) in
  fun b -> Resource.T (b, ops)

type two_way_ty = [source_ty | sink_ty | shutdown_ty]
type 'a two_way = ([> two_way_ty] as 'a) r

let shutdown (Resource.T (t, ops)) cmd =
  let module X = (val (Resource.get ops Shutdown)) in
  X.shutdown t cmd
