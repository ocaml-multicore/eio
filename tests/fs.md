# Setting up the environment

```ocaml
# #require "eio_main";;
# ignore @@ Unix.umask 0o022;;
- : unit = ()
```

```ocaml

module Path = Eio.Path

let () =
  Printexc.register_printer (function
    | Eio.Fs.Permission_denied (path, _) -> Some (Fmt.str "Eio.Fs.Permission_denied (%S, _)" path)
    | Eio.Fs.Already_exists (path, _)    -> Some (Fmt.str "Eio.Fs.Already_exists (%S, _)" path)
    | Eio.Fs.Not_found (path, _)         -> Some (Fmt.str "Eio.Fs.Not_found (%S, _)" path)
    | _ -> None
  )

open Eio.Std

let ( / ) = Path.( / )

let run (fn : Eio.Stdenv.t -> unit) =
  Eio_main.run @@ fun env ->
  fn env

let try_read_file path =
  match Path.load path with
  | s -> traceln "read %a -> %S" Path.pp path s
  | exception ex -> traceln "read %a -> %a" Path.pp path Fmt.exn ex

let try_write_file ~create ?append path content =
  match Path.save ~create ?append path content with
  | () -> traceln "write %a -> ok" Path.pp path
  | exception ex -> traceln "write %a -> %a" Path.pp path Fmt.exn ex

let try_mkdir path =
  match Path.mkdir path ~perm:0o700 with
  | () -> traceln "mkdir %a -> ok" Path.pp path
  | exception ex -> traceln "mkdir %a -> %a" Path.pp path Fmt.exn ex

let try_rename p1 p2 =
  match Path.rename p1 p2 with
  | () -> traceln "rename %a to %a -> ok" Path.pp p1 Path.pp p2
  | exception ex -> traceln "rename %a to %a -> %a" Path.pp p1 Path.pp p2 Fmt.exn ex

let try_read_dir path =
  match Path.read_dir path with
  | names -> traceln "read_dir %a -> %a" Path.pp path Fmt.Dump.(list string) names
  | exception ex -> traceln "read_dir %a -> %a" Path.pp path Fmt.exn ex

let try_unlink path =
  match Path.unlink path with
  | () -> traceln "unlink %a -> ok" Path.pp path
  | exception ex -> traceln "unlink %a -> %a" Path.pp path Fmt.exn ex

let try_rmdir path =
  match Path.rmdir path with
  | () -> traceln "rmdir %a -> ok" Path.pp path
  | exception ex -> traceln "rmdir %a -> %a" Path.pp path Fmt.exn ex

let chdir path =
  traceln "chdir %S" path;
  Unix.chdir path

let assert_kind path kind =
  Path.with_open_in path @@ fun file ->
  assert ((Eio.File.stat file).kind = kind)
```

# Basic test cases

Creating a file and reading it back:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o666) (cwd / "test-file") "my-data";
  traceln "Got %S" @@ Path.load (cwd / "test-file");;
+Got "my-data"
- : unit = ()
```

Check the file got the correct permissions (subject to the umask set above):
```ocaml
# Printf.printf "Perm = %o\n" ((Unix.stat "test-file").st_perm);;
Perm = 644
- : unit = ()
```

# Sandboxing

Trying to use cwd to access a file outside of that subtree fails:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o666) (cwd / "../test-file") "my-data";
  failwith "Should have failed";;
Exception: Eio.Fs.Permission_denied ("../test-file", _)
```

Trying to use cwd to access an absolute path fails:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o666) (cwd / "/tmp/test-file") "my-data";
  failwith "Should have failed";;
Exception: Eio.Fs.Permission_denied ("/tmp/test-file", _)
```

# Creation modes

Exclusive create fails if already exists:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o666) (cwd / "test-file") "first-write";
  Path.save ~create:(`Exclusive 0o666) (cwd / "test-file") "first-write";
  failwith "Should have failed";;
Exception: Eio.Fs.Already_exists ("test-file", _)
```

If-missing create succeeds if already exists:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  Path.save ~create:(`If_missing 0o666) test_file "1st-write-original";
  Path.save ~create:(`If_missing 0o666) test_file "2nd-write";
  traceln "Got %S" @@ Path.load test_file;;
+Got "2nd-write-original"
- : unit = ()
```

Truncate create succeeds if already exists, and truncates:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  Path.save ~create:(`Or_truncate 0o666) test_file "1st-write-original";
  Path.save ~create:(`Or_truncate 0o666) test_file "2nd-write";
  traceln "Got %S" @@ Path.load test_file;;
+Got "2nd-write"
- : unit = ()
# Unix.unlink "test-file";;
- : unit = ()
```

Error if no create and doesn't exist:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  Path.save ~create:`Never test_file "1st-write-original";
  traceln "Got %S" @@ Path.load test_file;;
Exception: Eio.Fs.Not_found ("test-file", _)
```

Appending to an existing file:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  Path.save ~create:(`Or_truncate 0o666) test_file "1st-write-original";
  Path.save ~create:`Never ~append:true test_file "2nd-write";
  traceln "Got %S" @@ Path.load test_file;;
+Got "1st-write-original2nd-write"
- : unit = ()
# Unix.unlink "test-file";;
- : unit = ()
```

# Mkdir

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "subdir");
  try_mkdir (cwd / "subdir/nested");
  Path.save ~create:(`Exclusive 0o600) (cwd / "subdir/nested/test-file") "data";
  ();;
+mkdir <cwd:subdir> -> ok
+mkdir <cwd:subdir/nested> -> ok
- : unit = ()
# Unix.unlink "subdir/nested/test-file"; Unix.rmdir "subdir/nested"; Unix.rmdir "subdir";;
- : unit = ()
```

Creating directories with nesting, symlinks, etc:
```ocaml
# Unix.symlink "/" "to-root";;
- : unit = ()
# Unix.symlink "subdir" "to-subdir";;
- : unit = ()
# Unix.symlink "foo" "dangle";;
- : unit = ()
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "subdir");
  try_mkdir (cwd / "to-subdir/nested");
  try_mkdir (cwd / "to-root/tmp/foo");
  try_mkdir (cwd / "../foo");
  try_mkdir (cwd / "to-subdir");
  try_mkdir (cwd / "dangle/foo");
  ();;
+mkdir <cwd:subdir> -> ok
+mkdir <cwd:to-subdir/nested> -> ok
+mkdir <cwd:to-root/tmp/foo> -> Eio.Fs.Permission_denied ("to-root/tmp/foo", _)
+mkdir <cwd:../foo> -> Eio.Fs.Permission_denied ("../foo", _)
+mkdir <cwd:to-subdir> -> Eio.Fs.Already_exists ("to-subdir", _)
+mkdir <cwd:dangle/foo> -> Eio.Fs.Not_found ("dangle/foo", _)
- : unit = ()
```

# Unlink

You can remove a file using unlink:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o600) (cwd / "file") "data";
  Path.save ~create:(`Exclusive 0o600) (cwd / "subdir/file2") "data2";
  try_read_file (cwd / "file");
  try_read_file (cwd / "subdir/file2");
  try_unlink (cwd / "file");
  try_unlink (cwd / "subdir/file2");
  try_read_file (cwd / "file");
  try_read_file (cwd / "subdir/file2");
  try_write_file ~create:(`Exclusive 0o600) (cwd / "subdir/file2") "data2";
  try_unlink (cwd / "to-subdir/file2");
  try_read_file (cwd / "subdir/file2");;
+read <cwd:file> -> "data"
+read <cwd:subdir/file2> -> "data2"
+unlink <cwd:file> -> ok
+unlink <cwd:subdir/file2> -> ok
+read <cwd:file> -> Eio.Fs.Not_found ("file", _)
+read <cwd:subdir/file2> -> Eio.Fs.Not_found ("subdir/file2", _)
+write <cwd:subdir/file2> -> ok
+unlink <cwd:to-subdir/file2> -> ok
+read <cwd:subdir/file2> -> Eio.Fs.Not_found ("subdir/file2", _)
- : unit = ()
```

Removing something that doesn't exist or is out of scope:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  try_unlink (cwd / "missing");
  try_unlink (cwd / "../foo");
  try_unlink (cwd / "to-subdir/foo");
  try_unlink (cwd / "to-root/foo");;
+unlink <cwd:missing> -> Eio.Fs.Not_found ("missing", _)
+unlink <cwd:../foo> -> Eio.Fs.Permission_denied ("../foo", _)
+unlink <cwd:to-subdir/foo> -> Eio.Fs.Not_found ("to-subdir/foo", _)
+unlink <cwd:to-root/foo> -> Eio.Fs.Permission_denied ("to-root/foo", _)
- : unit = ()
```

# Rmdir

Similar to `unlink`, but works on directories:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "d1");
  try_mkdir (cwd / "subdir/d2");
  try_read_dir (cwd / "d1");
  try_read_dir (cwd / "subdir/d2");
  try_rmdir (cwd / "d1");
  try_rmdir (cwd / "subdir/d2");
  try_read_dir (cwd / "d1");
  try_read_dir (cwd / "subdir/d2");
  try_mkdir (cwd / "subdir/d3");
  try_rmdir (cwd / "to-subdir/d3");
  try_read_dir (cwd / "subdir/d3");;
+mkdir <cwd:d1> -> ok
+mkdir <cwd:subdir/d2> -> ok
+read_dir <cwd:d1> -> []
+read_dir <cwd:subdir/d2> -> []
+rmdir <cwd:d1> -> ok
+rmdir <cwd:subdir/d2> -> ok
+read_dir <cwd:d1> -> Eio.Fs.Not_found ("d1", _)
+read_dir <cwd:subdir/d2> -> Eio.Fs.Not_found ("subdir/d2", _)
+mkdir <cwd:subdir/d3> -> ok
+rmdir <cwd:to-subdir/d3> -> ok
+read_dir <cwd:subdir/d3> -> Eio.Fs.Not_found ("subdir/d3", _)
- : unit = ()
```

Removing something that doesn't exist or is out of scope:

```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  try_rmdir (cwd / "missing");
  try_rmdir (cwd / "../foo");
  try_rmdir (cwd / "to-subdir/foo");
  try_rmdir (cwd / "to-root/foo");;
+rmdir <cwd:missing> -> Eio.Fs.Not_found ("missing", _)
+rmdir <cwd:../foo> -> Eio.Fs.Permission_denied ("../foo", _)
+rmdir <cwd:to-subdir/foo> -> Eio.Fs.Not_found ("to-subdir/foo", _)
+rmdir <cwd:to-root/foo> -> Eio.Fs.Permission_denied ("to-root/foo", _)
- : unit = ()
```

# Limiting to a subdirectory

Create a sandbox, write a file with it, then read it from outside:
```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "sandbox");
  let subdir = Path.open_dir ~sw (cwd / "sandbox") in
  Path.save ~create:(`Exclusive 0o600) (subdir / "test-file") "data";
  try_mkdir (subdir / "../new-sandbox");
  traceln "Got %S" @@ Path.load (cwd / "sandbox/test-file");;
+mkdir <cwd:sandbox> -> ok
+mkdir <sandbox:../new-sandbox> -> Eio.Fs.Permission_denied ("../new-sandbox", _)
+Got "data"
- : unit = ()
```

# Unconfined FS access

We create a directory and chdir into it.
Using `cwd` we can't access the parent, but using `fs` we can:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  try_mkdir (cwd / "fs-test");
  chdir "fs-test";
  Fun.protect ~finally:(fun () -> chdir "..") (fun () ->
    try_mkdir (cwd / "../outside-cwd");
    try_write_file ~create:(`Exclusive 0o600) (cwd / "../test-file") "data";
    try_mkdir (fs / "../outside-cwd");
    try_write_file ~create:(`Exclusive 0o600) (fs / "../test-file") "data";
  );
  Unix.unlink "test-file";
  Unix.rmdir "outside-cwd";;
+mkdir <cwd:fs-test> -> ok
+chdir "fs-test"
+mkdir <cwd:../outside-cwd> -> Eio.Fs.Permission_denied ("../outside-cwd", _)
+write <cwd:../test-file> -> Eio.Fs.Permission_denied ("../test-file", _)
+mkdir <fs:../outside-cwd> -> ok
+write <fs:../test-file> -> ok
+chdir ".."
- : unit = ()
```

Reading directory entries under `cwd` and outside of `cwd`.

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "readdir");
  Path.with_open_dir (cwd / "readdir") @@ fun tmpdir ->
  try_mkdir (tmpdir / "test-1");
  try_mkdir (tmpdir / "test-2");
  try_read_dir (tmpdir / ".");
  try_read_dir (tmpdir / "..");
  try_read_dir (tmpdir / "test-3");;
+mkdir <cwd:readdir> -> ok
+mkdir <readdir:test-1> -> ok
+mkdir <readdir:test-2> -> ok
+read_dir <readdir:.> -> ["test-1"; "test-2"]
+read_dir <readdir:..> -> Eio.Fs.Permission_denied ("..", _)
+read_dir <readdir:test-3> -> Eio.Fs.Not_found ("test-3", _)
- : unit = ()
```

Can use `fs` to access absolute paths:

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  let b = Buffer.create 10 in
  Path.with_open_in (fs / Filename.null) (fun flow -> Eio.Flow.copy flow (Eio.Flow.buffer_sink b));
  traceln "Read %S and got %S" Filename.null (Buffer.contents b);
  traceln "Trying with cwd instead fails:";
  Path.with_open_in (cwd / Filename.null) (fun flow -> Eio.Flow.copy flow (Eio.Flow.buffer_sink b));;;
+Read "/dev/null" and got ""
+Trying with cwd instead fails:
Exception: Eio.Fs.Permission_denied ("/dev/null", _)
```

## Streamling lines

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o600) (cwd / "test-data") "one\ntwo\nthree";
  Path.with_lines (cwd / "test-data") (fun lines ->
     Seq.iter (traceln "Line: %s") lines
  );;
+Line: one
+Line: two
+Line: three
- : unit = ()
```

# Unix interop

We can get the Unix FD from the flow and use it directly:

```ocaml
# run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Path.with_open_in (fs / Filename.null) (fun flow ->
     match Eio_unix.FD.peek_opt flow with
     | None -> failwith "No Unix file descriptor!"
     | Some fd ->
        let got = Unix.read fd (Bytes.create 10) 0 10 in
        traceln "Read %d bytes from null device" got
  );;
+Read 0 bytes from null device
- : unit = ()
```

We can also remove it from the flow completely and take ownership of it.
In that case, `with_open_in` will no longer close it on exit:

```ocaml
# run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let fd = Path.with_open_in (fs / Filename.null) (fun flow -> Option.get (Eio_unix.FD.take_opt flow)) in
  let got = Unix.read fd (Bytes.create 10) 0 10 in
  traceln "Read %d bytes from null device" got;
  Unix.close fd;;
+Read 0 bytes from null device
- : unit = ()
```

# Use after close

```ocaml
# run @@ fun env ->
  let closed = Switch.run (fun sw -> Path.open_dir ~sw env#cwd) in
  try
    failwith (Path.read_dir closed |> String.concat ",")
  with Invalid_argument _ -> traceln "Got Invalid_argument for closed FD";;
+Got Invalid_argument for closed FD
- : unit = ()
```

# Rename

```ocaml
let try_rename t =
  try_mkdir (t / "tmp");
  try_rename (t / "tmp") (t / "dir");
  try_write_file (t / "foo") "FOO" ~create:(`Exclusive 0o600);
  try_rename (t / "foo") (t / "dir/bar");
  try_read_file (t / "dir/bar");
  Path.with_open_dir (t / "dir") @@ fun dir ->
  try_rename (dir / "bar") (t / "foo");
  try_read_file (t / "foo");
  Unix.chdir "dir";
  try_rename (t / "../foo") (t / "foo");
  Unix.chdir ".."
```

Confined:

```ocaml
# run @@ fun env -> try_rename env#cwd;;
+mkdir <cwd:tmp> -> ok
+rename <cwd:tmp> to <cwd:dir> -> ok
+write <cwd:foo> -> ok
+rename <cwd:foo> to <cwd:dir/bar> -> ok
+read <cwd:dir/bar> -> "FOO"
+rename <dir:bar> to <cwd:foo> -> ok
+read <cwd:foo> -> "FOO"
+rename <cwd:../foo> to <cwd:foo> -> Eio.Fs.Permission_denied ("../foo", _)
- : unit = ()
```

Unconfined:

```ocaml
# run @@ fun env -> try_rename env#fs;;
+mkdir <fs:tmp> -> ok
+rename <fs:tmp> to <fs:dir> -> ok
+write <fs:foo> -> Eio.Fs.Already_exists ("foo", _)
+rename <fs:foo> to <fs:dir/bar> -> ok
+read <fs:dir/bar> -> "FOO"
+rename <dir:bar> to <fs:foo> -> ok
+read <fs:foo> -> "FOO"
+rename <fs:../foo> to <fs:foo> -> ok
- : unit = ()
```

# Stat
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Switch.run @@ fun sw ->
  try_mkdir (cwd / "stat_subdir");
  assert_kind (cwd / "stat_subdir") `Directory;
  try_write_file (cwd / "stat_reg") "kingbula" ~create:(`Exclusive 0o600);
  assert_kind (cwd / "stat_reg") `Regular_file;;
+mkdir <cwd:stat_subdir> -> ok
+write <cwd:stat_reg> -> ok
- : unit = ()
```
