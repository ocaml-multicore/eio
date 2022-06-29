open Eio.Std

type copy_method = [
  | `Read_into
  | `Read_source_buffer
]

type t = <
  Eio.Flow.two_way;
  Eio.Flow.close;
  on_read : string Handler.t;
  on_copy_bytes : int Handler.t;
  set_copy_method : copy_method -> unit;
  attach_to_switch : Switch.t -> unit;
>

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

let make ?(pp=pp_default) label =
  let on_read = Handler.make (`Raise End_of_file) in
  let on_copy_bytes = Handler.make (`Return 4096) in
  let copy_method = ref `Read_into in
  (* Test optimised copying using Read_source_buffer *)
  let copy_rsb_iovec src =
    let size = Handler.run on_copy_bytes in
    let len = min (Cstruct.lenv src) size in
    let bufs = takev len src in
    traceln "%s: wrote (rsb) @[<v>%a@]" label (Fmt.Dump.list (Fmt.using Cstruct.to_string pp)) bufs;
    len
  in
  let copy_rsb rsb =
    try while true do rsb copy_rsb_iovec done
    with End_of_file -> ()
  in
  (* Test fallback copy using buffer. *)
  let copy_via_buffer src =
    try
      while true do
        let size = Handler.run on_copy_bytes in
        let buf = Cstruct.create size in
        let n = Eio.Flow.read src buf in
        traceln "%s: wrote @[<v>%a@]" label pp (Cstruct.to_string (Cstruct.sub buf 0 n))
      done
    with End_of_file -> ()
  in
  object (self)
    inherit Eio.Flow.two_way

    val on_close = Queue.create ()

    method on_read = on_read
    method on_copy_bytes = on_copy_bytes

    method read_into buf =
      let data = Handler.run on_read in
      let len = String.length data in
      if Cstruct.length buf < len then
        Fmt.failwith "%s: read buffer too short for %a!" label pp data;
      Cstruct.blit_from_string data 0 buf 0 len;
      traceln "%s: read @[<v>%a@]" label pp data;
      len

    method copy src =
      match !copy_method with
      | `Read_into -> copy_via_buffer src
      | `Read_source_buffer ->
        let try_rsb = function
          | Eio.Flow.Read_source_buffer rsb -> copy_rsb rsb; true
          | _ -> false
        in
        if not (List.exists try_rsb (Eio.Flow.read_methods src)) then
          Fmt.failwith "Source does not offer Read_source_buffer optimisation"

    method set_copy_method m =
      copy_method := m

    method shutdown cmd =
      traceln "%s: shutdown %s" label @@
      match cmd with
      | `Receive -> "receive"
      | `Send -> "send"
      | `All -> "all"

    method attach_to_switch sw =
      let hook = Switch.on_release_cancellable sw (fun () -> Eio.Flow.close self) in
      Queue.add (fun () -> Eio.Switch.remove_hook hook) on_close

    method close =
      while not (Queue.is_empty on_close) do
        Queue.take on_close ()
      done;
      traceln "%s: closed" label
  end

let on_read (t:t) = Handler.seq t#on_read
let on_copy_bytes (t:t) = Handler.seq t#on_copy_bytes
let set_copy_method (t:t) = t#set_copy_method
let attach_to_switch (t:t) = t#attach_to_switch
