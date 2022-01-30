# Setting up the environment

```ocaml
# #require "eio_main";;
# ignore @@ Unix.umask 0o022;;
- : unit = ()
```

```ocaml
let () =
  Printexc.register_printer (function
    | Eio.Dir.Permission_denied (path, _) -> Some (Fmt.str "Eio.Dir.Permission_denied (%S, _)" path)
    | Eio.Dir.Already_exists (path, _)    -> Some (Fmt.str "Eio.Dir.Already_exists (%S, _)" path)
    | Eio.Dir.Not_found (path, _)         -> Some (Fmt.str "Eio.Dir.Not_found (%S, _)" path)
    | _ -> None
  )

open Eio.Std

let run (fn : Eio.Stdenv.t -> unit) =
  Eio_main.run @@ fun env ->
  fn env

let try_write_file ~create ?append dir path content =
  match Eio.Dir.save ~create ?append dir path content with
  | () -> traceln "write %S -> ok" path
  | exception ex -> traceln "write %S -> %a" path Fmt.exn ex

let try_mkdir dir path =
  match Eio.Dir.mkdir dir path ~perm:0o700 with
  | () -> traceln "mkdir %S -> ok" path
  | exception ex -> traceln "mkdir %S -> %a" path Fmt.exn ex

let chdir path =
  traceln "chdir %S" path;
  Unix.chdir path
```

# Basic test cases

Creating a file and reading it back:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`Exclusive 0o666) cwd "test-file" "my-data";
  traceln "Got %S" @@ Eio.Dir.load cwd "test-file";;
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
  Eio.Dir.save ~create:(`Exclusive 0o666) cwd "../test-file" "my-data";
  failwith "Should have failed";;
Exception: Eio.Dir.Permission_denied ("../test-file", _)
```

Trying to use cwd to access an absolute path fails:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`Exclusive 0o666) cwd "/tmp/test-file" "my-data";
  failwith "Should have failed";;
Exception: Eio.Dir.Permission_denied ("/tmp/test-file", _)
```

# Creation modes

Exclusive create fails if already exists:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`Exclusive 0o666) cwd "test-file" "first-write";
  Eio.Dir.save ~create:(`Exclusive 0o666) cwd "test-file" "first-write";
  failwith "Should have failed";;
Exception: Eio.Dir.Already_exists ("test-file", _)
```

If-missing create succeeds if already exists:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`If_missing 0o666) cwd "test-file" "1st-write-original";
  Eio.Dir.save ~create:(`If_missing 0o666) cwd "test-file" "2nd-write";
  traceln "Got %S" @@ Eio.Dir.load cwd "test-file";;
+Got "2nd-write-original"
- : unit = ()
```

Truncate create succeeds if already exists, and truncates:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`Or_truncate 0o666) cwd "test-file" "1st-write-original";
  Eio.Dir.save ~create:(`Or_truncate 0o666) cwd "test-file" "2nd-write";
  traceln "Got %S" @@ Eio.Dir.load cwd "test-file";;
+Got "2nd-write"
- : unit = ()
# Unix.unlink "test-file";;
- : unit = ()
```

Error if no create and doesn't exist:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:`Never cwd "test-file" "1st-write-original";
  traceln "Got %S" @@ Eio.Dir.load cwd "test-file";;
Exception: Eio.Dir.Not_found ("test-file", _)
```

Appending to an existing file:
```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`Or_truncate 0o666) cwd "test-file" "1st-write-original";
  Eio.Dir.save ~create:`Never ~append:true cwd "test-file" "2nd-write";
  traceln "Got %S" @@ Eio.Dir.load cwd "test-file";;
+Got "1st-write-original2nd-write"
- : unit = ()
# Unix.unlink "test-file";;
- : unit = ()
```

# Mkdir

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir cwd "subdir";
  try_mkdir cwd "subdir/nested";
  Eio.Dir.save ~create:(`Exclusive 0o600) cwd "subdir/nested/test-file" "data";
  ();;
+mkdir "subdir" -> ok
+mkdir "subdir/nested" -> ok
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
  try_mkdir cwd "subdir";
  try_mkdir cwd "to-subdir/nested";
  try_mkdir cwd "to-root/tmp/foo";
  try_mkdir cwd "../foo";
  try_mkdir cwd "to-subdir";
  try_mkdir cwd "dangle/foo";
  ();;
+mkdir "subdir" -> ok
+mkdir "to-subdir/nested" -> ok
+mkdir "to-root/tmp/foo" -> Eio.Dir.Permission_denied ("to-root/tmp/foo", _)
+mkdir "../foo" -> Eio.Dir.Permission_denied ("../foo", _)
+mkdir "to-subdir" -> Eio.Dir.Already_exists ("to-subdir", _)
+mkdir "dangle/foo" -> Eio.Dir.Not_found ("dangle", _)
- : unit = ()
```

# Limiting to a subdirectory

Create a sandbox, write a file with it, then read it from outside:
```ocaml
# run @@ fun env ->
  Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir cwd "sandbox";
  let subdir = Eio.Dir.open_dir ~sw cwd "sandbox" in
  Eio.Dir.save ~create:(`Exclusive 0o600) subdir "test-file" "data";
  try_mkdir subdir "../new-sandbox";
  traceln "Got %S" @@ Eio.Dir.load cwd "sandbox/test-file";;
+mkdir "sandbox" -> ok
+mkdir "../new-sandbox" -> Eio.Dir.Permission_denied ("../new-sandbox", _)
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
  try_mkdir cwd "fs-test";
  chdir "fs-test";
  Fun.protect ~finally:(fun () -> chdir "..") (fun () ->
    try_mkdir cwd "../outside-cwd";
    try_write_file ~create:(`Exclusive 0o600) cwd "../test-file" "data";
    try_mkdir fs "../outside-cwd";
    try_write_file ~create:(`Exclusive 0o600) fs "../test-file" "data";
  );
  Unix.unlink "test-file";
  Unix.rmdir "outside-cwd";;
+mkdir "fs-test" -> ok
+chdir "fs-test"
+mkdir "../outside-cwd" -> Eio.Dir.Permission_denied ("../outside-cwd", _)
+write "../test-file" -> Eio.Dir.Permission_denied ("../test-file", _)
+mkdir "../outside-cwd" -> ok
+write "../test-file" -> ok
+chdir ".."
- : unit = ()
```

Can use `fs` to access absolute paths:

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  let b = Buffer.create 10 in
  Eio.Dir.with_open_in fs Filename.null (fun flow -> Eio.Flow.copy flow (Eio.Flow.buffer_sink b));
  traceln "Read %S and got %S" Filename.null (Buffer.contents b);
  traceln "Trying with cwd instead fails:";
  Eio.Dir.with_open_in cwd Filename.null (fun flow -> Eio.Flow.copy flow (Eio.Flow.buffer_sink b));;;
+Read "/dev/null" and got ""
+Trying with cwd instead fails:
Exception: Eio.Dir.Permission_denied ("/dev/null", _)
```

## Streamling lines

```ocaml
# run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Eio.Dir.save ~create:(`Exclusive 0o600) cwd "test-data" "one\ntwo\nthree";
  Eio.Dir.with_lines cwd "test-data" (fun lines ->
     Seq.iter (traceln "Line: %s") lines
  );;
+Line: one
+Line: two
+Line: three
- : unit = ()
```
