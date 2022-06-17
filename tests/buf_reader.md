```ocaml
# #require "eio";;
# #require "eio.mock";;
```
```ocaml
module R = Eio.Buf_read;;
open R.Syntax;;

let traceln fmt = Fmt.pr ("+" ^^ fmt ^^ "@.")

let peek t =
  let s = Cstruct.to_string (R.peek t) in
  assert (String.length s = R.buffered_bytes t);
  s

let ensure t n =
  R.ensure t n;
  peek t

(* The next data to be returned by `mock_flow`. `[]` to raise `End_of_file`: *)
let next = ref []

let mock_flow = object
  inherit Eio.Flow.source

  method read_methods = []

  method read_into buf =
    match !next with
    | [] ->
      traceln "mock_flow returning Eof";
      raise End_of_file
    | x :: xs ->
      let len = min (Cstruct.length buf) (String.length x) in
      traceln "mock_flow returning %d bytes" len;
      Cstruct.blit_from_string x 0 buf 0 len;
      let x' = String.sub x len (String.length x - len) in
      next := (if x' = "" then xs else x' :: xs);
      len
end

let read flow n =
  let buf = Cstruct.create n in
  let len = Eio.Flow.read flow buf in
  traceln "Read %S" (Cstruct.to_string buf ~len)

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let test ?(max_size=10) input p =
  next := input;
  let i = R.of_flow mock_flow ~max_size in
  p i

let parse_exn p flow ~max_size =
  match R.parse_exn p flow ~max_size with
  | x -> traceln "Ok: %S" x
  | exception Failure msg -> traceln "Failure: %s" msg
```


## A simple run-through

```ocaml
# let i = R.of_flow (Eio.Flow.string_source "Hello") ~max_size:100;;
val i : R.t = <abstr>
# peek i;;
- : string = ""
# ensure i 1;;
- : string = "Hello"
# R.consume i 1;;
- : unit = ()
# peek i;;
- : string = "ello"
# ensure i 4;;
- : string = "ello"
# ensure i 5;;
Exception: End_of_file.
# peek i;;
- : string = "ello"
# R.consume i 4;;
- : unit = ()
# peek i;;
- : string = ""
```

## Minimising reads on the underlying flow

```ocaml
# let i = R.of_flow mock_flow ~initial_size:4 ~max_size:10;;
val i : R.t = <abstr>
```

The first read fills the initial buffer:
```ocaml
# next := ["hello world!"]; ensure i 1;;
+mock_flow returning 4 bytes
- : string = "hell"
```

The next read forces a resize (doubling to 8):
```ocaml
# ensure i 5;;
+mock_flow returning 4 bytes
- : string = "hello wo"
```

Now the buffer is at max-size (10):
```ocaml
# ensure i 9;;
+mock_flow returning 2 bytes
- : string = "hello worl"
# ensure i 10;;
- : string = "hello worl"
# ensure i 11;;
Exception: Eio__Buf_read.Buffer_limit_exceeded.
```

Sometimes, doubling isn't enough. Here we go straight to 10 bytes:

```ocaml
# let i = R.of_flow mock_flow ~initial_size:4 ~max_size:10;;
val i : R.t = <abstr>
# next := ["hello world!"]; ensure i 10;;
+mock_flow returning 10 bytes
- : string = "hello worl"
```

## End-of-file

After getting end-of-file, we don't use the flow any more:

```ocaml
# let i = R.of_flow mock_flow ~initial_size:4 ~max_size:10;;
val i : R.t = <abstr>
# next := ["hi"]; ensure i 10;;
+mock_flow returning 2 bytes
+mock_flow returning Eof
Exception: End_of_file.
# peek i;;
- : string = "hi"
# ensure i 10;;
Exception: End_of_file.
# R.take_all i;;
- : string = "hi"
# R.take_all i;;
- : string = ""
```

## Multiple reads

We might need several reads to fulfill the user's request:
```ocaml
# let i = R.of_flow mock_flow ~initial_size:4 ~max_size:10;;
val i : R.t = <abstr>
# next := ["one"; "two"; "three"]; ensure i 10;;
+mock_flow returning 3 bytes
+mock_flow returning 3 bytes
+mock_flow returning 4 bytes
- : string = "onetwothre"
# R.consume i 4; ensure i 7;;
+mock_flow returning 1 bytes
- : string = "wothree"
```

## Reading lines

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["one"; "\ntwo\n"; "three\n"]; R.line i;;
+mock_flow returning 3 bytes
+mock_flow returning 5 bytes
- : string = "one"
# R.line i;;
- : string = "two"
# R.line i;;
+mock_flow returning 6 bytes
- : string = "three"
# R.line i;;
+mock_flow returning Eof
Exception: End_of_file.
```

DOS lines:

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["one\r"; "\ntwo\r\n"; "three\r\n"]; R.line i;;
+mock_flow returning 4 bytes
+mock_flow returning 6 bytes
- : string = "one"
# R.line i;;
- : string = "two"
# R.line i;;
+mock_flow returning 7 bytes
- : string = "three"
# R.line i;;
+mock_flow returning Eof
Exception: End_of_file.
```

Missing EOL:

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["on"; "e\ntwo"]; R.line i;;
+mock_flow returning 2 bytes
+mock_flow returning 5 bytes
- : string = "one"
# R.line i;;
+mock_flow returning Eof
- : string = "two"
```

Multiple lines in one read:

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["one\ntwo\n\nthree"]; R.line i;;
+mock_flow returning 14 bytes
- : string = "one"
# R.line i;;
- : string = "two"
# R.line i;;
- : string = ""
# R.line i;;
+mock_flow returning Eof
- : string = "three"
# R.line i;;
Exception: End_of_file.
```

## Flow interface

```ocaml
# let bflow = R.of_flow mock_flow ~max_size:100 |> R.as_flow;;
val bflow : Eio.Flow.source = <obj>
# next := ["foo"; "bar"]; read bflow 2;;
+mock_flow returning 3 bytes
+Read "fo"
- : unit = ()
# read bflow 2;;
+Read "o"
- : unit = ()
# read bflow 2;;
+mock_flow returning 3 bytes
+Read "ba"
- : unit = ()
# read bflow 2;;
+Read "r"
- : unit = ()
# read bflow 2;;
+mock_flow returning Eof
Exception: End_of_file.
```

## Characters

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>

# next := ["ab"; "c"]; R.any_char i;;
+mock_flow returning 2 bytes
- : char = 'a'

# R.peek_char i;;
- : char option = Some 'b'

# R.any_char i;;
- : char = 'b'

# R.any_char i;;
+mock_flow returning 1 bytes
- : char = 'c'

# R.any_char i;;
+mock_flow returning Eof
Exception: End_of_file.

# R.peek_char i;;
- : char option = None
```

## Fixed-length strings

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["ab"; "c"]; R.take 1 i;;
+mock_flow returning 2 bytes
- : string = "a"
# R.take 3 i;;
+mock_flow returning 1 bytes
+mock_flow returning Eof
Exception: End_of_file.
# R.take 2 i;;
- : string = "bc"
```

## Literals

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["ab"; "c"]; R.char 'A' i;;
+mock_flow returning 2 bytes
Exception: Failure "Expected 'A' but got 'a'".
# R.char 'a' i;;
- : unit = ()
# R.string "BC" i;;
Exception: Failure "Expected \"BC\" but got \"b\"".
# R.string "bC" i;;
+mock_flow returning 1 bytes
Exception: Failure "Expected \"bC\" but got \"bc\"".
# R.string "bcd" i;;
+mock_flow returning Eof
Exception: End_of_file.
# R.string "bcd" i;;
Exception: End_of_file.
# R.string "bc" i;;
- : unit = ()
# peek i;;
- : string = ""
```

## Scanning

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>

# next := ["aa"; "a0"; "123de"]; R.skip_while ((=) 'a') i;;
+mock_flow returning 2 bytes
+mock_flow returning 2 bytes
- : unit = ()

# R.take_while is_digit i;;
+mock_flow returning 5 bytes
- : string = "0123"

# R.take_while (Fun.negate is_digit) i;;
+mock_flow returning Eof
- : string = "de"

# test ["abc"; "def"; "ghi"] (R.skip 5 *> R.take_all);;
+mock_flow returning 3 bytes
+mock_flow returning 3 bytes
+mock_flow returning 3 bytes
+mock_flow returning Eof
- : string = "fghi"

# test ~max_size:3 ["abcdefg"] (R.skip 5 *> R.take_all);;
+mock_flow returning 3 bytes
+mock_flow returning 3 bytes
+mock_flow returning 1 bytes
+mock_flow returning Eof
- : string = "fg"
```

## Take all

```ocaml
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# next := ["20 text/gemini\r\n"; "# Introduction\n"; "# Conclusion\n"]; R.line i;;
+mock_flow returning 16 bytes
- : string = "20 text/gemini"
# R.take_all i;;
+mock_flow returning 15 bytes
+mock_flow returning 13 bytes
+mock_flow returning Eof
- : string = "# Introduction\n# Conclusion\n"
```

```ocaml
# let i = R.of_flow mock_flow ~max_size:10;;
val i : R.t = <abstr>
# next := ["abc"; "def"; "ghi"; "jkl"]; R.take_all i;;
+mock_flow returning 3 bytes
+mock_flow returning 3 bytes
+mock_flow returning 3 bytes
+mock_flow returning 1 bytes
Exception: Eio__Buf_read.Buffer_limit_exceeded.
# R.take 3 i;;
- : string = "abc"
```

## Combinators

Parsers can be combined in the usual ways:

```ocaml
# test ["abc"] (R.map String.uppercase_ascii (R.take 2));;
+mock_flow returning 3 bytes
- : string = "AB"

# test ["abc"] (R.pair R.any_char R.take_all);;
+mock_flow returning 3 bytes
+mock_flow returning Eof
- : char * string = ('a', "bc")

# test ["abc"] (R.bind R.any_char R.char);;
+mock_flow returning 3 bytes
Exception: Failure "Expected 'a' but got 'b'".
```

Syntax:

```ocaml
# test ["abc"] (let+ x = R.take 2 in String.uppercase_ascii x);;
+mock_flow returning 3 bytes
- : string = "AB"

# test ["abc"] (let+ x = R.any_char and+ y = R.take_all in (x, y));;
+mock_flow returning 3 bytes
+mock_flow returning Eof
- : char * string = ('a', "bc")

# test ["abc"] (let* x = R.any_char in R.char x);;
+mock_flow returning 3 bytes
Exception: Failure "Expected 'a' but got 'b'".

# test ["aac"] (let* x = R.any_char in R.char x *> R.take_all);;
+mock_flow returning 3 bytes
+mock_flow returning Eof
- : string = "c"

# test ["ab"] (R.any_char <* R.any_char);;
+mock_flow returning 2 bytes
- : char = 'a'

# test ["ab"] (R.any_char *> R.any_char);;
+mock_flow returning 2 bytes
- : char = 'b'
```

## Error handling

```ocaml
# test ["abc"] R.(format_errors (take 3));;
+mock_flow returning 3 bytes
- : (string, [> `Msg of string ]) result = Ok "abc"

# test ["abc"] R.(format_errors (take 2 <* end_of_input));;
+mock_flow returning 3 bytes
- : (string, [> `Msg of string ]) result =
Error (`Msg "Unexpected data after parsing (at offset 2)")

# test ["abc"] R.(format_errors (take 4 <* end_of_input));;
+mock_flow returning 3 bytes
+mock_flow returning Eof
- : (string, [> `Msg of string ]) result =
Error (`Msg "Unexpected end-of-file at offset 3")

# test ~max_size:2 ["abc"] R.(format_errors line);;
+mock_flow returning 2 bytes
- : (string, [> `Msg of string ]) result =
Error (`Msg "Buffer size limit exceeded when reading at offset 0")
```

## Sequences

```ocaml
# test ["one"; "\ntwo\n"; "three"] R.lines |> Seq.iter (traceln "%S");;
+mock_flow returning 3 bytes
+mock_flow returning 5 bytes
+"one"
+"two"
+mock_flow returning 5 bytes
+mock_flow returning Eof
+"three"
- : unit = ()

# test ["abcd1234"] R.(seq (take 2)) |> List.of_seq |> String.concat ",";;
+mock_flow returning 8 bytes
+mock_flow returning Eof
- : string = "ab,cd,12,34"

# test ["abcd123"] R.(seq (take 2)) |> List.of_seq |> String.concat ",";;
+mock_flow returning 7 bytes
+mock_flow returning Eof
Exception: End_of_file.
```

A sequence node remembers its offset and fails if used out of sequence:

```ocaml
# next := ["one"; "\ntwo\n"; "three"];;
- : unit = ()
# let i = R.of_flow mock_flow ~max_size:100;;
val i : R.t = <abstr>
# let seq = R.lines i;;
val seq : string Seq.t = <fun>
# let line, seq' = match seq () with Cons (a, b) -> (a, b) | _ -> assert false;;
+mock_flow returning 3 bytes
+mock_flow returning 5 bytes
val line : string = "one"
val seq' : string Seq.t = <fun>

# seq ();;
Exception:
Invalid_argument
 "Sequence is stale (expected to be used at offset 0, but stream is now at 4)".

# seq' ();;
- : string Seq.node = Seq.Cons ("two", <fun>)

# seq' ();;
Exception:
Invalid_argument
 "Sequence is stale (expected to be used at offset 4, but stream is now at 8)".
```

## Convenience wrapper

`parse` turns parser errors into friendly messages:

```ocaml
# R.(parse (string "FROM:" *> take_all)) (Eio.Flow.string_source "FROM:A") ~max_size:5;;
- : (string, [> `Msg of string ]) result = Ok "A"

# R.(parse (string "FROM:" *> take_all)) (Eio.Flow.string_source "TO:B") ~max_size:5;;
- : (string, [> `Msg of string ]) result =
Error (`Msg "Expected \"FROM:\" but got \"TO:B\" (at offset 0)")

# R.(parse (string "FROM:" *> take_all)) (Eio.Flow.string_source "FROM:ABCDE") ~max_size:5;;
- : (string, [> `Msg of string ]) result =
Error (`Msg "Buffer size limit exceeded when reading at offset 5")

# R.(parse (string "END")) (Eio.Flow.string_source "ENDING") ~max_size:5;;
- : (unit, [> `Msg of string ]) result =
Error (`Msg "Unexpected data after parsing (at offset 3)")

# R.(parse (string "END")) (Eio.Flow.string_source "E") ~max_size:5;;
- : (unit, [> `Msg of string ]) result =
Error (`Msg "Unexpected end-of-file at offset 1")
```

`parse_exn` is similar, but raises (we then catch it and print it nicely):

```ocaml
# parse_exn R.(string "FROM:" *> take_all) (Eio.Flow.string_source "FROM:A") ~max_size:5;;
+Ok: "A"
- : unit = ()

# parse_exn R.(string "FROM:" *> take_all) (Eio.Flow.string_source "TO:B") ~max_size:5;;
+Failure: Expected "FROM:" but got "TO:B" (at offset 0)
- : unit = ()

# parse_exn R.(string "FROM:" *> take_all) (Eio.Flow.string_source "FROM:ABCDE") ~max_size:5;;
+Failure: Buffer size limit exceeded when reading at offset 5
- : unit = ()

# parse_exn R.(take 3) (Eio.Flow.string_source "ENDING") ~max_size:5;;
+Failure: Unexpected data after parsing (at offset 3)
- : unit = ()

# parse_exn R.(take 3) (Eio.Flow.string_source "E") ~max_size:5;;
+Failure: Unexpected end-of-file at offset 1
- : unit = ()
```

## Parsing strings

There are some convenience functions for parsing strings:

```ocaml
# let r = R.of_string "hello\nworld\n";;
val r : R.t = <abstr>
# R.line r;;
- : string = "hello"
# R.line r;;
- : string = "world"
# R.line r;;
Exception: End_of_file.
```

```ocaml
# R.parse_string R.line "foo\n";;
- : (string, [> `Msg of string ]) result = Ok "foo"

# R.parse_string R.line "foo\nbar\n";;
- : (string, [> `Msg of string ]) result =
Error (`Msg "Unexpected data after parsing (at offset 4)")

# R.parse_string_exn R.line "foo\n";;
- : string = "foo"

# R.parse_string_exn R.line "foo\nbar\n";;
Exception: Failure "Unexpected data after parsing (at offset 4)".
```

## Test using mock flow

```ocaml
# let flow = Eio_mock.Flow.make "flow" in
  Eio_mock.Flow.on_read flow [
    `Return "foo\nba";
    `Return "r\n";
    `Raise End_of_file;
  ];
  R.parse_exn ~max_size:100 R.(line <*> line) flow;;
+flow: read "foo\n"
+           "ba"
+flow: read "r\n"
- : string * string = ("foo", "bar")
```
