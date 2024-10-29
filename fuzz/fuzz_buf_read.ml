(* This file contains a simple model of `Buf_read`, using a single string.
   It runs random operations on both the model and the real buffer and
   checks they always give the same result. *)

module String = struct
  include String

  let rec find ?(start=0) p t =
    if start = String.length t then None
    else if p t.[start] then Some start
    else find ~start:(succ start) p t

  let drop t n = String.sub t n (String.length t - n)

  let cut ~sep t =
    match String.index_opt t sep with
    | None -> None
    | Some i -> Some (String.sub t 0 i, drop t (i + 1))
end

let debug = false

module Buf_read = Eio.Buf_read
exception Buffer_limit_exceeded = Buf_read.Buffer_limit_exceeded

let initial_size = 10
let max_size = 100

module Mock_flow = struct
  type t = string list ref

  let rec single_read t buf =
    match !t with
    | [] ->
      raise End_of_file
    | "" :: xs ->
      t := xs;
      single_read t buf
    | x :: xs ->
      let len = min (Cstruct.length buf) (String.length x) in
      Cstruct.blit_from_string x 0 buf 0 len;
      let x' = String.drop x len in
      t := (if x' = "" then xs else x' :: xs);
      len

  let read_methods = []
end

let mock_flow =
  let ops = Eio.Flow.Pi.source (module Mock_flow) in
  fun chunks -> Eio.Resource.T (ref chunks, ops)

module Model = struct
  type t = string ref

  let of_chunks chunks = ref (String.concat "" chunks)

  let take_all t =
    let old = !t in
    if String.length old >= max_size then raise Buffer_limit_exceeded;
    t := "";
    old

  let line t =
    match String.cut ~sep:'\n' !t with
    | Some (line, rest) ->
      if String.length line >= max_size then raise Buffer_limit_exceeded;
      t := rest;
      if String.ends_with ~suffix:"\r" line then String.sub line 0 (String.length line - 1)
      else line
    | None when !t = "" -> raise End_of_file
    | None when String.length !t >= max_size -> raise Buffer_limit_exceeded
    | None -> take_all t

  let any_char t =
    match !t with
    | "" -> raise End_of_file
    | s ->
      t := String.drop s 1;
      s.[0]

  let peek_char t =
    match !t with
    | "" -> None
    | s -> Some (s.[0])

  let consume t n =
    t := String.drop !t n

  let char c t =
    match peek_char t with
    | Some c2 when c = c2 -> consume t 1
    | Some _ -> failwith "char"
    | None -> raise End_of_file

  let string s t =
    if debug then Fmt.pr "string %S@." s;
    let len_t = String.length !t in
    let prefix = String.sub s 0 (min len_t (String.length s)) in
    if not (String.starts_with ~prefix !t) then failwith "string";
    if String.length s > max_size then raise Buffer_limit_exceeded;
    if String.starts_with ~prefix:s !t then consume t (String.length s)
    else raise End_of_file

  let take n t =
    if n < 0 then invalid_arg "neg";
    if n > max_size then raise Buffer_limit_exceeded
    else if String.length !t >= n then (
      let data = String.sub !t 0 n in
      t := String.drop !t n;
      data
    ) else raise End_of_file

  let take_while p t =
    match String.find (Fun.negate p) !t with
    | Some i when i >= max_size -> raise Buffer_limit_exceeded
    | Some i ->
      let data = String.sub !t 0 i in
      consume t i;
      data
    | None -> take_all t

  let skip_while p t =
    match String.find (Fun.negate p) !t with
    | Some i -> consume t i
    | None -> t := ""

  let skip n t =
    if n < 0 then invalid_arg "skip";
    if n > String.length !t then (
      t := "";
      raise End_of_file;
    );
    consume t n

  let end_of_input t =
    if !t <> "" then failwith "not eof"

  let rec lines t =
    match line t with
    | line -> line :: lines t
    | exception End_of_file -> []

  module BE = struct
    let uint16 t = String.get_uint16_be (take 2 t) 0

    let uint32 t = String.get_int32_be (take 4 t) 0

    let uint48 t =
      let s = take 6 t in
      let upper_16 = String.get_uint16_be s 0 |> Int64.of_int in
      let middle_16 = String.get_uint16_be s 2 |> Int64.of_int in
      let lower_16 = String.get_uint16_be s 4 |> Int64.of_int in
      Int64.(
        add 
          (shift_left upper_16 32)
        (add
          (shift_left middle_16 16)
          (lower_16))
      )

    let uint64 t = String.get_int64_be (take 8 t) 0

    let float t =
      Int32.float_of_bits (
        String.get_int32_be (take 4 t) 0)

    let double t =
      Int64.float_of_bits (
        String.get_int64_be (take 8 t) 0)
  end

  module LE = struct
    let uint16 t = String.get_uint16_le (take 2 t) 0

    let uint32 t = String.get_int32_le (take 4 t) 0

    let uint48 t =
      let s = take 6 t in
      let lower_16 = String.get_uint16_le s 0 |> Int64.of_int in
      let middle_16 = String.get_uint16_le s 2 |> Int64.of_int in
      let upper_16 = String.get_uint16_le s 4 |> Int64.of_int in
      Int64.(
        add 
          (shift_left upper_16 32)
        (add
          (shift_left middle_16 16)
          (lower_16))
      )

    let uint64 t = String.get_int64_le (take 8 t) 0

    let float t =
      Int32.float_of_bits (
        String.get_int32_le (take 4 t) 0)

    let double t =
      Int64.float_of_bits (
        String.get_int64_le (take 8 t) 0)
  end
end

type op = Op : 'a Crowbar.printer * 'a Buf_read.parser * (Model.t -> 'a) -> op

let unit = Fmt.(const string) "()"
let dump_char f c = Fmt.pf f "%C" c

let digit = function
  | '0'..'9' -> true
  | _ -> false

let op =
  let label (name, gen) = Crowbar.with_printer Fmt.(const string name) gen in
  Crowbar.choose @@ List.map label [
    "line", Crowbar.const @@ Op (Fmt.Dump.string, Buf_read.line, Model.line);
    "char 'x'", Crowbar.const @@ Op (unit, Buf_read.char 'x', Model.char 'x');
    "any_char", Crowbar.const @@ Op (dump_char, Buf_read.any_char, Model.any_char);
    "peek_char", Crowbar.const @@ Op (Fmt.Dump.option dump_char, Buf_read.peek_char, Model.peek_char);
    "string", Crowbar.(map [bytes]) (fun s -> Op (unit, Buf_read.string s, Model.string s));
    "take", Crowbar.(map [int]) (fun n -> Op (Fmt.Dump.string, Buf_read.take n, Model.take n));
    "take_all", Crowbar.const @@ Op (Fmt.Dump.string, Buf_read.take_all, Model.take_all);
    "take_while digit", Crowbar.const @@ Op (Fmt.Dump.string, Buf_read.take_while digit, Model.take_while digit);
    "skip_while digit", Crowbar.const @@ Op (unit, Buf_read.skip_while digit, Model.skip_while digit);
    "skip", Crowbar.(map [int]) (fun n -> Op (unit, Buf_read.skip n, Model.skip n));
    "end_of_input", Crowbar.const @@ Op (unit, Buf_read.end_of_input, Model.end_of_input);
    "lines", Crowbar.const @@ Op (Fmt.Dump.(list string), (Buf_read.(map List.of_seq lines)), Model.lines);
    "be_uint16", Crowbar.const @@ Op (Fmt.int, (Buf_read.BE.uint16), Model.BE.uint16);
    "be_uint32", Crowbar.const @@ Op (Fmt.int32, (Buf_read.BE.uint32), Model.BE.uint32);
    "be_uint48", Crowbar.const @@ Op (Fmt.int64, (Buf_read.BE.uint48), Model.BE.uint48);
    "be_uint64", Crowbar.const @@ Op (Fmt.int64, (Buf_read.BE.uint64), Model.BE.uint64);
    "be_float", Crowbar.const @@ Op (Fmt.float, (Buf_read.BE.float), Model.BE.float);
    "be_double", Crowbar.const @@ Op (Fmt.float, (Buf_read.BE.double), Model.BE.double);
    "le_uint16", Crowbar.const @@ Op (Fmt.int, (Buf_read.LE.uint16), Model.LE.uint16);
    "le_uint32", Crowbar.const @@ Op (Fmt.int32, (Buf_read.LE.uint32), Model.LE.uint32);
    "le_uint48", Crowbar.const @@ Op (Fmt.int64, (Buf_read.LE.uint48), Model.LE.uint48);
    "le_uint64", Crowbar.const @@ Op (Fmt.int64, (Buf_read.LE.uint64), Model.LE.uint64);
    "le_float", Crowbar.const @@ Op (Fmt.float, (Buf_read.LE.float), Model.LE.float);
    "le_double", Crowbar.const @@ Op (Fmt.float, (Buf_read.LE.double), Model.LE.double);
  ]

let catch f x =
  match f x with
  | y -> Ok y
  | exception End_of_file -> Error "EOF"
  | exception Invalid_argument _ -> Error "Invalid"
  | exception Failure _ -> Error "Failure"
  | exception Buffer_limit_exceeded -> Error "TooBig"

let random chunks ops =
  let model = Model.of_chunks chunks in
  let chunks_len = String.length !model in
  let r = Buf_read.of_flow (mock_flow chunks) ~initial_size ~max_size in
  if debug then print_endline "*** start ***";
  let check_eq (Op (pp, a, b)) =
    if debug then (
      Fmt.pr "---@.";
      Fmt.pr "real :%S@." (Cstruct.to_string (Buf_read.peek r));
      Fmt.pr "model:%S@." !model;
    );
    let x = catch a r in
    let y = catch b model in
    Crowbar.check_eq ~pp:Fmt.(result ~ok:pp ~error:string) x y
  in
  List.iter check_eq ops;
  Crowbar.check_eq ~pp:Fmt.int (Buf_read.consumed_bytes r) (chunks_len - String.length !model)

let () =
  Crowbar.(add_test ~name:"random ops" [list bytes; list op] random)
