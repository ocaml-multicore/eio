```ocaml
# #require "eio";;
```
```ocaml
module R = Eio.Buf_read;;

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

let mock_flow = object (_ : #Eio.Flow.read)
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
  let len = Eio.Flow.read_into flow buf in
  traceln "Read %S" (Cstruct.to_string buf ~len)

let is_digit = function
  | '0'..'9' -> true
  | _ -> false
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
val bflow : Eio.Flow.read = <obj>
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
# R.any_char i;;
- : char = 'b'
# R.any_char i;;
+mock_flow returning 1 bytes
- : char = 'c'
# R.any_char i;;
+mock_flow returning Eof
Exception: End_of_file.
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
```
