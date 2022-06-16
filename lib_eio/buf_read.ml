exception Buffer_limit_exceeded

type t = {
  mutable buf : Cstruct.buffer;
  mutable pos : int;
  mutable len : int;
  mutable flow : Flow.source option;    (* None if we've seen eof *)
  mutable consumed : int;               (* Total bytes consumed so far *)
  max_size : int;
}

type 'a parser = t -> 'a

let return = Fun.const

let map f x r = f (x r)

let pair x y r =
  let a = x r in
  let b = y r in
  a, b

let bind x f r = f (x r) r

module Syntax = struct
  let ( let+ ) x f r = f (x r)
  let ( let* ) = bind
  let ( and* ) = pair
  let ( and+ ) = pair

  let ( <* ) a b t =
    let x = a t in
    ignore (b t);
    x

  let ( *> ) a b t =
    ignore (a t);
    b t
end

open Syntax

let capacity t = Bigarray.Array1.dim t.buf

let of_flow ?initial_size ~max_size flow =
  let flow = (flow :> Flow.source) in
  if max_size <= 0 then Fmt.invalid_arg "Max size %d should be positive!" max_size;
  let initial_size = Option.value initial_size ~default:(min 4096 max_size) in
  let buf = Bigarray.(Array1.create char c_layout initial_size) in
  { buf; pos = 0; len = 0; flow = Some flow; max_size; consumed = 0 }

let of_buffer buf =
  let len = Bigarray.Array1.dim buf in
  { buf; pos = 0; len; flow = None; max_size = max_int; consumed = 0 }

let of_string s =
  let len = String.length s in
  let buf = Bigarray.(Array1.create char c_layout) len in
  Cstruct.blit_from_string s 0 (Cstruct.of_bigarray buf) 0 len;
  of_buffer buf

let peek t =
  Cstruct.of_bigarray ~off:t.pos ~len:t.len t.buf

let consume t n =
  if n < 0 || n > t.len then Fmt.invalid_arg "Can't consume %d bytes of a %d byte buffer!" n t.len;
  t.pos <- t.pos + n;
  t.len <- t.len - n;
  t.consumed <- t.consumed + n

let consume_all t =
  t.consumed <- t.consumed + t.len;
  t.len <- 0

let buffered_bytes t = t.len

let consumed_bytes t = t.consumed

let eof_seen t = t.flow = None

let ensure t n =
  assert (n >= 0);
  if t.len < n then (
    if n > t.max_size then raise Buffer_limit_exceeded;
    (* We don't have enough data yet, so we'll need to do a read. *)
    match t.flow with
    | None -> raise End_of_file
    | Some flow ->
      (* If the buffer is empty, we might as well use all of it: *)
      if t.len = 0 then t.pos <- 0;
      let () =
        let cap = capacity t in
        if n > cap then (
          (* [n] bytes won't fit. We need to resize the buffer. *)
          let new_size = max n (min t.max_size (cap * 2)) in
          let new_buf = Bigarray.(Array1.create char c_layout new_size) in
          Cstruct.blit
            (peek t) 0
            (Cstruct.of_bigarray new_buf) 0
            t.len;
          t.pos <- 0;
          t.buf <- new_buf
        ) else if t.pos + n > cap then (
          (* [n] bytes will fit in the existing buffer, but we need to compact it first. *)
          Cstruct.blit
            (peek t) 0
            (Cstruct.of_bigarray t.buf) 0
            t.len;
          t.pos <- 0
        )
      in
      try
        while t.len < n do
          let free_space = Cstruct.of_bigarray t.buf ~off:(t.pos + t.len) in
          assert (t.len + Cstruct.length free_space >= n);
          let got = Flow.read flow free_space in
          t.len <- t.len + got
        done
      with End_of_file ->
        t.flow <- None;
        raise End_of_file
  );
  assert (buffered_bytes t >= n)

let as_flow t =
  object
    inherit Flow.source

    method read_into dst =
      ensure t 1;
      let len = min (buffered_bytes t) (Cstruct.length dst) in
      Cstruct.blit (peek t) 0 dst 0 len;
      consume t len;
      len
  end

let get t i =
  Bigarray.Array1.get t.buf (t.pos + i)

let char c t =
  ensure t 1;
  let c2 = get t 0 in
  if c <> c2 then Fmt.failwith "Expected %C but got %C" c c2;
  consume t 1

let any_char t =
  ensure t 1;
  let c = get t 0 in
  consume t 1;
  c

let peek_char t =
  match ensure t 1 with
  | () -> Some (get t 0)
  | exception End_of_file -> None

let take len t =
  if len < 0 then Fmt.invalid_arg "take: %d is negative!" len;
  ensure t len;
  let data = Cstruct.to_string (Cstruct.of_bigarray t.buf ~off:t.pos ~len) in
  consume t len;
  data

let string s t =
  let rec aux i =
    if i = String.length s then (
      consume t i
    ) else if i < t.len then (
      if get t i = s.[i] then aux (i + 1)
      else (
        let buf = peek t in
        let len = min (String.length s) (Cstruct.length buf) in
        Fmt.failwith "Expected %S but got %S"
          s
          (Cstruct.to_string buf ~off:0 ~len)
      )
    ) else (
      ensure t (t.len + 1);
      aux i
    )
  in
  aux 0

let take_all t =
  try
    while true do ensure t (t.len + 1) done;
    assert false
  with End_of_file ->
    let data = Cstruct.to_string (peek t) in
    consume t t.len;
    data

let count_while p t =
  let rec aux i =
    if i < t.len then (
      if p (get t i) then aux (i + 1)
      else i
    ) else (
      ensure t (t.len + 1);
      aux i
    )
  in
  try aux 0
  with End_of_file -> t.len

let take_while p t =
  let len = count_while p t in
  let data = Cstruct.to_string (Cstruct.of_bigarray t.buf ~off:t.pos ~len) in
  consume t len;
  data

let skip_while p t =
  let rec aux i =
    if i < t.len then (
      if p (get t i) then aux (i + 1)
      else consume t i
    ) else (
      consume t t.len;
      ensure t 1;
      aux 0
    )
  in
  try aux 0
  with End_of_file -> ()

let rec skip n t =
  if n <= t.len then (
    consume t n
  ) else (
    let n = n - t.len in
    consume_all t;
    ensure t (min n (capacity t));
    skip n t
  )

let skip n t =
  if n < 0 then Fmt.invalid_arg "skip: %d is negative!" n;
  try skip n t
  with End_of_file ->
    (* Skip isn't atomic, so discard everything in this case for consistency. *)
    consume t t.len;
    raise End_of_file

let line t =
  (* Return the index of the first '\n', reading more data as needed. *)
  let rec aux i =
    if i = t.len then (
      ensure t (t.len + 1);
      aux i
    ) else if get t i = '\n' then (
      i
    ) else (
      aux (i + 1)
    )
  in
  match aux 0 with
  | exception End_of_file when t.len > 0 -> take_all t
  | nl ->
    let len =
      if nl > 0 && get t (nl - 1) = '\r' then nl - 1
      else nl
    in
    let line = Cstruct.to_string (Cstruct.of_bigarray t.buf ~off:t.pos ~len) in
    consume t (nl + 1);
    line

let at_end_of_input t =
  if t.len = 0 && eof_seen t then true
  else (
    match ensure t 1 with
    | () -> false
    | exception End_of_file -> true
  )

let end_of_input t =
  if not (at_end_of_input t) then
    failwith "Unexpected data after parsing"

let pp_pos f t =
  Fmt.pf f "at offset %d" (consumed_bytes t)

let format_errors p t =
  match p t with
  | v -> Ok v
  | exception Failure msg -> Fmt.error_msg "%s (%a)" msg pp_pos t
  | exception End_of_file -> Fmt.error_msg "Unexpected end-of-file at offset %d" (t.consumed + t.len)
  | exception Buffer_limit_exceeded -> Fmt.error_msg "Buffer size limit exceeded when reading %a" pp_pos t

let parse ?initial_size ~max_size p flow =
  let buf = of_flow flow ?initial_size ~max_size in
  format_errors (p <* end_of_input) buf

let parse_exn ?initial_size ~max_size p flow =
  match parse ?initial_size ~max_size p flow with
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let parse_string p s =
  format_errors (p <* end_of_input) (of_string s)

let parse_string_exn p s =
  match parse_string p s with
  | Ok x -> x
  | Error (`Msg m) -> failwith m

[@@inline never]
let bad_offset ~expected actual =
  Fmt.invalid_arg "Sequence is stale (expected to be used at offset %d, but stream is now at %d)"
    expected actual

let seq ?(stop=at_end_of_input) p t =
  let rec aux offset () =
    if offset <> t.consumed then bad_offset ~expected:offset t.consumed;
    if stop t then Seq.Nil
    else (
      let item = p t in
      Seq.Cons (item, aux t.consumed)
    )
  in
  aux t.consumed

let lines t = seq line t
