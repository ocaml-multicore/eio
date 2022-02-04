(* This file contains a simple model of `Buf_read`, using a single string.
   It runs random operations on both the model and the real buffer and
   checks they always give the same result. *)

open Astring

let debug = false

module Buf_read = Eio.Buf_read
exception Buffer_limit_exceeded = Buf_read.Buffer_limit_exceeded

let initial_size = 10
let max_size = 100

let mock_flow next = object (self)
  inherit Eio.Flow.source

  val mutable next = next

  method read_into buf =
    match next with
    | [] ->
      raise End_of_file
    | "" :: xs ->
      next <- xs;
      self#read_into buf
    | x :: xs ->
      let len = min (Cstruct.length buf) (String.length x) in
      Cstruct.blit_from_string x 0 buf 0 len;
      let x' = String.with_index_range x ~first:len in
      next <- (if x' = "" then xs else x' :: xs);
      len
end

module Model = struct
  type t = string ref

  let of_chunks chunks = ref (String.concat chunks)

  let take_all t =
    let old = !t in
    if String.length old >= max_size then raise Buffer_limit_exceeded;
    t := "";
    old

  let line t =
    match String.cut ~sep:"\n" !t with
    | Some (line, rest) ->
      if String.length line >= max_size then raise Buffer_limit_exceeded;
      t := rest;
      if String.is_suffix ~affix:"\r" line then String.with_index_range line ~last:(String.length line - 2)
      else line
    | None when !t = "" -> raise End_of_file
    | None when String.length !t >= max_size -> raise Buffer_limit_exceeded
    | None -> take_all t

  let any_char t =
    match !t with
    | "" -> raise End_of_file
    | s ->
      t := String.with_index_range s ~first:1;
      String.get_head s

  let peek_char t = String.head !t

  let consume t n =
    t := String.with_index_range !t ~first:n

  let char c t =
    match peek_char t with
    | Some c2 when c = c2 -> consume t 1
    | Some _ -> failwith "char"
    | None -> raise End_of_file

  let string s t =
    if debug then Fmt.pr "string %S@." s;
    let len_t = String.length !t in
    if not (String.is_prefix ~affix:(String.with_range s ~len:len_t) !t) then failwith "string";
    if String.length s > max_size then raise Buffer_limit_exceeded;
    if String.is_prefix ~affix:s !t then consume t (String.length s)
    else raise End_of_file

  let take n t =
    if n < 0 then invalid_arg "neg";
    if n > max_size then raise Buffer_limit_exceeded
    else if String.length !t >= n then (
      let data = String.with_range !t ~len:n in
      t := String.with_range !t ~first:n;
      data
    ) else raise End_of_file

  let take_while p t =
    match String.find (Fun.negate p) !t with
    | Some i when i >= max_size -> raise Buffer_limit_exceeded
    | Some i ->
      let data = String.with_range !t ~len:i in
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
