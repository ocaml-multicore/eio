exception Buffer_limit_exceeded

type t = {
  mutable buf : Cstruct.buffer;
  mutable pos : int;
  mutable len : int;
  mutable flow : Flow.read option;      (* None if we've seen eof *)
  max_size : int;
}

type 'a parser = t -> 'a

let capacity t = Bigarray.Array1.dim t.buf

let of_flow ?initial_size ~max_size flow =
  let flow = (flow :> Flow.read) in
  if max_size <= 0 then Fmt.invalid_arg "Max size %d should be positive!" max_size;
  let initial_size = Option.value initial_size ~default:(min 4096 max_size) in
  let buf = Bigarray.(Array1.create char c_layout initial_size) in
  { buf; pos = 0; len = 0; flow = Some flow; max_size }

let peek t =
  Cstruct.of_bigarray ~off:t.pos ~len:t.len t.buf

let consume t n =
  if n < 0 || n > t.len then Fmt.invalid_arg "Can't consume %d bytes of a %d byte buffer!" n t.len;
  t.pos <- t.pos + n;
  t.len <- t.len - n

let buffered_bytes t = t.len

let eof_seen t = t.flow = None

let ensure t n =
  assert (n >= 0);
  if t.len < n then (
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
          if n > t.max_size then raise Buffer_limit_exceeded;
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
          let got = Flow.read_into flow free_space in
          t.len <- t.len + got
        done
      with End_of_file ->
        t.flow <- None;
        raise End_of_file
  );
  assert (buffered_bytes t >= n)

let as_flow t =
  object (_ : Flow.read)
    method read_methods = []

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

let take len t =
  ensure t len;
  let data = Cstruct.to_string (Cstruct.of_bigarray t.buf ~off:t.pos ~len) in
  consume t len;
  data

let string s t =
  let rec aux i =
    if i = String.length s then true
    else if i < t.len then (
      if get t i = s.[i] then aux (i + 1)
      else false
    ) else (
      ensure t (t.len + 1);
      aux i
    )
  in
  if not (aux 0) then (
    let buf = peek t in
    let len = min (String.length s) (Cstruct.length buf) in
    Fmt.failwith "Expected %S but got %S"
      s
      (Cstruct.to_string buf ~off:0 ~len)
  )

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
  let len = count_while p t in
  consume t len

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
