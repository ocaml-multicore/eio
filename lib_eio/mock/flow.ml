open Eio.Std

type copy_method = [
  | `Read_into
  | `Read_source_buffer
]

module Mock_flow = struct
  type tag = [`Generic | `Mock]

  type t = {
    label : string;
    pp : string Fmt.t;
    on_close : (unit -> unit) Queue.t;
    on_read : string Handler.t;
    on_copy_bytes : int Handler.t;
    mutable copy_method : copy_method;
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

  let rec takev len = function
    | [] -> []
    | x :: _ when Cstruct.length x >= len -> [Cstruct.sub x 0 len]
    | x :: xs -> x :: takev (len - Cstruct.length x) xs

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

  let copy_rsb_iovec t bufs =
    let pp f bufs = Fmt.pf f "(rsb) @[<v>%a@]" (Fmt.Dump.list (Fmt.using Cstruct.to_string t.pp)) bufs in
    write ~pp t bufs

  (* Test optimised copying using Read_source_buffer *)
  let copy_rsb t rsb =
    try while true do rsb (copy_rsb_iovec t) done
    with End_of_file -> ()

  (* Test fallback copy using buffer. *)
  let copy_via_buffer t src =
    try
      while true do
        let size = Handler.run t.on_copy_bytes in
        let buf = Cstruct.create size in
        let n = Eio.Flow.single_read src buf in
        traceln "%s: wrote @[<v>%a@]" t.label t.pp (Cstruct.to_string (Cstruct.sub buf 0 n))
      done
    with End_of_file -> ()

  let read_methods = []

  let single_read t buf =
    let data = Handler.run t.on_read in
    let len = String.length data in
    if Cstruct.length buf < len then
      Fmt.failwith "%s: read buffer too short for %a!" t.label t.pp data;
    Cstruct.blit_from_string data 0 buf 0 len;
    traceln "%s: read @[<v>%a@]" t.label t.pp data;
    len

  let copy t ~src =
    match t.copy_method with
    | `Read_into -> copy_via_buffer t src
    | `Read_source_buffer ->
      let Eio.Resource.T (src, ops) = src in
      let module Src = (val (Eio.Resource.get ops Eio.Flow.Pi.Source)) in
      let try_rsb = function
        | Eio.Flow.Read_source_buffer rsb -> copy_rsb t (rsb src); true
        | _ -> false
      in
      if not (List.exists try_rsb Src.read_methods) then
        Fmt.failwith "Source does not offer Read_source_buffer optimisation"

  let shutdown t cmd =
    traceln "%s: shutdown %s" t.label @@
    match cmd with
    | `Receive -> "receive"
    | `Send -> "send"
    | `All -> "all"

  let close t =
    while not (Queue.is_empty t.on_close) do
      Queue.take t.on_close ()
    done;
    traceln "%s: closed" t.label

  let make ?(pp=pp_default) label =
    {
      pp;
      label;
      on_close = Queue.create ();
      on_read = Handler.make (`Raise End_of_file);
      on_copy_bytes = Handler.make (`Return 4096);
      copy_method = `Read_into;
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
let set_copy_method t v = (raw t).copy_method <- v

let handler = Eio.Resource.handler (
    H (Type, Fun.id) ::
    Eio.Resource.bindings (Eio.Net.Pi.stream_socket (module Mock_flow))
  )

let make ?pp label : t =
  Eio.Resource.T (Mock_flow.make ?pp label, handler)
