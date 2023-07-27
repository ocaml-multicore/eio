# Setting up the environment

```ocaml
# #require "eio_main";;
# ignore @@ Unix.umask 0o022;;
- : unit = ()
```

```ocaml

module Int63 = Optint.Int63
module Path = Eio.Path

let () = Eio.Exn.Backend.show := false

open Eio.Std

let ( / ) = Path.( / )

let run fn =
  Eio_main.run @@ fun env ->
  fn env

let try_read_file path =
  match Path.load path with
  | s -> traceln "read %a -> %S" Path.pp path s
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_write_file ~create ?append path content =
  match Path.save ~create ?append path content with
  | () -> traceln "write %a -> ok" Path.pp path
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_mkdir path =
  match Path.mkdir path ~perm:0o700 with
  | () -> traceln "mkdir %a -> ok" Path.pp path
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_mkdirs ?exists_ok path =
  match Path.mkdirs ?exists_ok path ~perm:0o700 with
  | () -> traceln "mkdirs %a -> ok" Path.pp path
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_rename p1 p2 =
  match Path.rename p1 p2 with
  | () -> traceln "rename %a to %a -> ok" Path.pp p1 Path.pp p2
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_read_dir path =
  match Path.read_dir path with
  | names -> traceln "read_dir %a -> %a" Path.pp path Fmt.Dump.(list string) names
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_unlink path =
  match Path.unlink path with
  | () -> traceln "unlink %a -> ok" Path.pp path
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_rmdir path =
  match Path.rmdir path with
  | () -> traceln "rmdir %a -> ok" Path.pp path
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

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
Exception: Eio.Io Fs Permission_denied _,
  opening <cwd:../test-file>
```

Trying to use cwd to access an absolute path fails:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o666) (cwd / "/tmp/test-file") "my-data";
  failwith "Should have failed";;
Exception: Eio.Io Fs Permission_denied _,
  opening <cwd:/tmp/test-file>
```

# Creation modes

Exclusive create fails if already exists:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o666) (cwd / "test-file") "first-write";
  Path.save ~create:(`Exclusive 0o666) (cwd / "test-file") "first-write";
  failwith "Should have failed";;
Exception: Eio.Io Fs Already_exists _,
  opening <cwd:test-file>
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
Exception: Eio.Io Fs Not_found _,
  opening <cwd:test-file>
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
+Eio.Io Fs Permission_denied _, creating directory <cwd:to-root/tmp/foo>
+Eio.Io Fs Permission_denied _, creating directory <cwd:../foo>
+Eio.Io Fs Already_exists _, creating directory <cwd:to-subdir>
+Eio.Io Fs Not_found _, creating directory <cwd:dangle/foo>
- : unit = ()
```

# Mkdirs

Recursively creating directories with `mkdirs`.

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let nested = cwd / "subdir1" / "subdir2" / "subdir3" in
  try_mkdirs nested;
  let one_more = Path.(nested / "subdir4") in
  try_mkdirs one_more;
  try_mkdirs ~exists_ok:true one_more;
  try_mkdirs (cwd / ".." / "outside");;
+mkdirs <cwd:subdir1/subdir2/subdir3> -> ok
+Eio.Io Fs Already_exists _, creating directory <cwd:subdir1>
+mkdirs <cwd:subdir1/subdir2/subdir3/subdir4> -> ok
+Eio.Io Fs Permission_denied _, creating directory <cwd:..>
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
+Eio.Io Fs Not_found _, opening <cwd:file>
+Eio.Io Fs Not_found _, opening <cwd:subdir/file2>
+write <cwd:subdir/file2> -> ok
+unlink <cwd:to-subdir/file2> -> ok
+Eio.Io Fs Not_found _, opening <cwd:subdir/file2>
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
+Eio.Io Fs Not_found _, removing file <cwd:missing>
+Eio.Io Fs Permission_denied _, removing file <cwd:../foo>
+Eio.Io Fs Not_found _, removing file <cwd:to-subdir/foo>
+Eio.Io Fs Permission_denied _, removing file <cwd:to-root/foo>
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
+Eio.Io Fs Not_found _, reading directory <cwd:d1>
+Eio.Io Fs Not_found _, reading directory <cwd:subdir/d2>
+mkdir <cwd:subdir/d3> -> ok
+rmdir <cwd:to-subdir/d3> -> ok
+Eio.Io Fs Not_found _, reading directory <cwd:subdir/d3>
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
+Eio.Io Fs Not_found _, removing directory <cwd:missing>
+Eio.Io Fs Permission_denied _, removing directory <cwd:../foo>
+Eio.Io Fs Not_found _, removing directory <cwd:to-subdir/foo>
+Eio.Io Fs Permission_denied _, removing directory <cwd:to-root/foo>
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
+Eio.Io Fs Permission_denied _, creating directory <sandbox:../new-sandbox>
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
+Eio.Io Fs Permission_denied _, creating directory <cwd:../outside-cwd>
+Eio.Io Fs Permission_denied _, opening <cwd:../test-file>
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
  try_read_dir tmpdir;
  try_read_dir (tmpdir / ".");
  try_read_dir (tmpdir / "..");
  try_read_dir (tmpdir / "test-3");;
+mkdir <cwd:readdir> -> ok
+mkdir <readdir:test-1> -> ok
+mkdir <readdir:test-2> -> ok
+read_dir <readdir> -> ["test-1"; "test-2"]
+read_dir <readdir:.> -> ["test-1"; "test-2"]
+Eio.Io Fs Permission_denied _, reading directory <readdir:..>
+Eio.Io Fs Not_found _, reading directory <readdir:test-3>
- : unit = ()
```

An error from the underlying directory, not the sandbox:

```ocaml
# Unix.mkdir "test-no-access" 0;;
- : unit = ()
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_read_dir (cwd / "test-no-access");;
+Eio.Io Fs Permission_denied _, reading directory <cwd:test-no-access>
- : unit = ()
# Unix.chmod "test-no-access" 0o700;;
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
Exception: Eio.Io Fs Permission_denied _,
  opening <cwd:/dev/null>
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
     match Eio_unix.Resource.fd_opt flow with
     | None -> failwith "No Unix file descriptor!"
     | Some fd ->
        Eio_unix.Fd.use_exn "read" fd @@ fun fd ->
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
  let fd = Path.with_open_in (fs / Filename.null) (fun flow ->
    Option.get (Eio_unix.Fd.remove (Option.get (Eio_unix.Resource.fd_opt flow)))
  ) in
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
+Eio.Io Fs Permission_denied _, renaming <cwd:../foo> to <cwd:foo>
- : unit = ()
```

Unconfined:

```ocaml
# run @@ fun env -> try_rename env#fs;;
+mkdir <fs:tmp> -> ok
+rename <fs:tmp> to <fs:dir> -> ok
+Eio.Io Fs Already_exists _, opening <fs:foo>
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

# pread/pwrite

Check reading and writing vectors at arbitrary offsets:

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let path = cwd / "test.txt" in
  Path.with_open_out path ~create:(`Exclusive 0o600) @@ fun file ->
  Eio.Flow.copy_string "+-!" file;
  Eio.File.pwrite_all file ~file_offset:(Int63.of_int 2) Cstruct.[of_string "abc"; of_string "123"];
  let buf1 = Cstruct.create 3 in
  let buf2 = Cstruct.create 4 in
  Eio.File.pread_exact file ~file_offset:(Int63.of_int 1) [buf1; buf2];
  traceln" %S/%S" (Cstruct.to_string buf1) (Cstruct.to_string buf2);;
+ "-ab"/"c123"
- : unit = ()
```

Reading at the end of a file:

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let path = cwd / "test.txt" in
  Path.with_open_out path ~create:(`Or_truncate 0o600) @@ fun file ->
  Eio.Flow.copy_string "abc" file;
  let buf = Cstruct.create 10 in
  let got = Eio.File.pread file [buf] ~file_offset:(Int63.of_int 0) in
  traceln "Read %S" (Cstruct.to_string buf ~len:got);
  try
    ignore (Eio.File.pread file [buf] ~file_offset:(Int63.of_int 3) : int);
    assert false
  with End_of_file ->
    traceln "End-of-file";;
+Read "abc"
+End-of-file
- : unit = ()
```

# Cancelling while readable

Ensure reads can be cancelled promptly, even if there is no need to wait:

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Path.with_open_out (env#fs / "/dev/zero") ~create:`Never @@ fun null ->
  Fiber.both
     (fun () ->
        let buf = Cstruct.create 4 in
        for _ = 1 to 10 do Eio.Flow.read_exact null buf done;
        assert false)
     (fun () -> failwith "Simulated error");;
Exception: Failure "Simulated error".
```
