open Eio.Std

let rec takev len = function
  | [] -> []
  | x :: _ when Cstruct.length x >= len ->
    let len = min len (Cstruct.length x) in
    if len = 0 then [] else [Cstruct.sub x 0 len]
  | x :: xs -> x :: takev (len - Cstruct.length x) xs

module Mock_flow = struct
  type tag = [`Generic | `Mock]

  type t = {
    label : string;
    pp : string Fmt.t;
    on_close : (unit -> unit) Queue.t;
    on_read : string Handler.t;
    on_copy_bytes : int Handler.t;
  }

  let pp_default f s =
    let rec aux i =
      let nl =
        match String.index_from_opt s i '\n' with
        | None -> String.length s
        | Some x -> x + 1
      in
      Fmt.Dump.string f (String.sub s i (nl - i));
      if nl < String.length s then (
        Fmt.cut f ();
        aux nl
      )
    in
    aux 0

  let write ~pp t bufs =
    let size = Handler.run t.on_copy_bytes in
    let len = min (Cstruct.lenv bufs) size in
    let bufs = takev len bufs in
    traceln "%s: wrote %a" t.label pp bufs;
    len

  let single_write t bufs =
    let pp f = function
      | [buf] -> Fmt.pf f "@[<v>%a@]" t.pp (Cstruct.to_string buf)
      | bufs -> Fmt.pf f "@[<v>%a@]" (Fmt.Dump.list (Fmt.using Cstruct.to_string t.pp)) bufs
    in
    write ~pp t bufs

  let read_methods = []

  let single_read t buf =
    let data = Handler.run t.on_read in
    let len = String.length data in
    if Cstruct.length buf < len then
      Fmt.failwith "%s: read buffer too short for %a!" t.label t.pp data;
    Cstruct.blit_from_string data 0 buf 0 len;
    traceln "%s: read @[<v>%a@]" t.label t.pp data;
    len

  let copy t ~src = Eio.Flow.Pi.simple_copy t ~src ~single_write

  let shutdown t cmd =
    traceln "%s: shutdown %s" t.label @@
    match cmd with
    | `Receive -> "receive"
    | `Send -> "send"
    | `All -> "all"

  let close t =
    while not (Queue.is_empty t.on_close) do
      (Queue.take t.on_close) ()
    done;
    traceln "%s: closed" t.label

  let setsockopt t opt v = Sockopt.setsockopt t.label opt v
  let getsockopt t opt = Sockopt.getsockopt t.label opt

  let make ?(pp=pp_default) label =
    {
      pp;
      label;
      on_close = Queue.create ();
      on_read = Handler.make (`Raise End_of_file);
      on_copy_bytes = Handler.make (`Return 4096);
    }
end

type ty = [`Generic | `Mock] Eio.Net.stream_socket_ty

type t = ty r

type (_, _, _) Eio.Resource.pi += Type : ('t, 't -> Mock_flow.t, ty) Eio.Resource.pi
let raw (Eio.Resource.T (t, ops)) = Eio.Resource.get ops Type t

let attach_to_switch t sw =
  let t = raw t in
  let hook = Switch.on_release_cancellable sw (fun () -> Mock_flow.close t) in
  Queue.add (fun () -> Eio.Switch.remove_hook hook) t.on_close

let on_read t = Handler.seq (raw t).on_read
let on_copy_bytes t = Handler.seq (raw t).on_copy_bytes

let handler = Eio.Resource.handler (
    H (Type, Fun.id) ::
    Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module Mock_flow))
  )

let make ?pp label : t =
  Eio.Resource.T (Mock_flow.make ?pp label, handler)

let trace_reads label src =
  let Eio.Resource.T (t, ops) = src in
  let module X = (val (Eio.Resource.get ops Eio.Flow.Pi.Source)) in
  let module Impl = struct
    include X

    let trace_rsb rsb t fn =
      rsb t (fun bufs ->
        let n = fn bufs in
        traceln "%s: shift rsb %d" label n;
        n
      )

    let read_methods =
      X.read_methods |> List.map (function
        | Eio.Flow.Read_source_buffer fn -> Eio.Flow.Read_source_buffer (trace_rsb fn)
        | x -> x)

    let single_read t dst =
      match X.single_read t dst with
      | len ->
        traceln "%s: read %d -> %S" label (Cstruct.length dst) (Cstruct.to_string dst ~len);
        len
      | exception End_of_file ->
        traceln "%s: read -> End_of_file" label;
        raise End_of_file
  end in
  let ops = Eio.Flow.Pi.source (module Impl) in
  Eio.Resource.T (t, ops)

let limit_reads sizes src =
  let next_limit = Seq.to_dispenser (List.to_seq sizes) in
  let Eio.Resource.T (t, ops) = src in
  let module X = (val (Eio.Resource.get ops Eio.Flow.Pi.Source)) in
  let module Impl = struct
    include X

    let limitv bufs =
      match next_limit () with
      | None -> bufs
      | Some n -> takev n bufs

    let limit_size buf =
      match next_limit () with
      | None -> buf
      | Some x -> Cstruct.sub buf 0 (min x (Cstruct.length buf))

    let limit_rsb rsb t fn =
      rsb t (fun bufs -> fn (limitv bufs))

    let read_methods =
      X.read_methods |> List.map (function
        | Eio.Flow.Read_source_buffer fn -> Eio.Flow.Read_source_buffer (limit_rsb fn)
        | x -> x)

    let single_read t dst =
      X.single_read t (limit_size dst)
  end in
  let ops = Eio.Flow.Pi.source (module Impl) in
  Eio.Resource.T (t, ops)
