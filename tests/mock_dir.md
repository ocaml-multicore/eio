# Mock directories

A mock directory uses either POSIX or Windows path syntax,
so path handling can be tested for both operating systems irrespective
of the host.

## Setting up the environment

```ocaml
# #require "eio.mock";;
```

```ocaml
open Eio.Std

let ( / ) = Eio.Path.( / )

let show p = traceln "%a" Eio.Path.pp p

let show_split p =
  match Eio.Path.split p with
  | None -> traceln "%a -> (none)" Eio.Path.pp p
  | Some (dir, leaf) -> traceln "%a -> %a + %S" Eio.Path.pp p Eio.Path.pp dir leaf

let posix_dir = Eio_mock.Dir.make "posix"
let win_dir = Eio_mock.Dir.make ~syntax:`Windows "win"

(* Like [Eio.Stdenv.cwd], a directory is used at the root of a path: *)
let posix = (posix_dir, "")
let win = (win_dir, "")
```

## POSIX syntax

Appending and replacing the path with an absolute one:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  show (posix / "foo");
  show (posix / "foo" / "bar");
  show (posix / "foo" / "/etc/passwd");;
+<posix:foo>
+<posix:foo/bar>
+<posix:/etc/passwd>
- : unit = ()
```

Splitting:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  show_split (posix / "foo" / "bar");
  show_split (posix / "foo");
  show_split (posix / "/foo/bar/");
  show_split (posix / "/");;
+<posix:foo/bar> -> <posix:foo> + "bar"
+<posix:foo> -> <posix> + "foo"
+<posix:/foo/bar/> -> <posix:/foo> + "bar"
+<posix:/> -> (none)
- : unit = ()
```

## Windows syntax

A drive-absolute path replaces the old one and then further steps are joined:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  show (win / "foo" / "bar");
  show (win / "C:\\foo" / "bar");;
+<win:foo\\bar>
+<win:C:\\foo\\bar>
- : unit = ()
```

A bare drive is drive-relative, so no separator is added:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  show (win / "C:" / "foo");;
+<win:C:foo>
- : unit = ()
```

Splitting never splits the volume prefix:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  show_split (win / "C:\\foo\\bar");
  show_split (win / "C:\\foo");
  show_split (win / "C:\\");;
+<win:C:\\foo\\bar> -> <win:C:\\foo> + "bar"
+<win:C:\\foo> -> <win:C:\\> + "foo"
+<win:C:\\> -> (none)
- : unit = ()
```

Verbatim paths keep using backslashes when joining:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  show (win / "\\\\?\\C:\\foo" / "bar");;
+<win:\\\\?\\C:\\foo\\bar>
- : unit = ()
```

## Operations

Operations that return a value must be configured:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio_mock.Dir.on_read_dir posix_dir [`Return ["b"; "a"]];
  let items = Eio.Path.read_dir (posix / "sub") in
  traceln "%a" Fmt.Dump.(list string) items;;
+posix: read_dir "sub"
+["a"; "b"]
- : unit = ()
```

Operations that just have side-effects are traced:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio.Path.mkdir ~perm:0o700 (posix / "sub" / "dir");
  Eio.Path.unlink (win / "C:\\tmp" / "x");
  Eio.Path.rename (posix / "a") (posix / "b");;
+posix: mkdir ~perm:0o700 "sub/dir"
+win: unlink "C:\\tmp\\x"
+posix: rename "a" to <posix:b>
- : unit = ()
```

Mock directories have no OS-native paths:

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  Eio.Path.native_exn (posix / "foo");;
Exception: Eio.Io Fs Not_native "<posix:foo>"
```
