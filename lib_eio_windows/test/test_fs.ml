module Int63 = Optint.Int63
module Path = Eio.Path

let () = Eio.Exn.Backend.show := false

open Eio.Std

let ( / ) = Path.( / )

let try_read_file path =
  match Path.load path with
  | s -> traceln "read %a -> %S" Path.pp path s
  | exception ex -> raise ex

let try_write_file ~create ?append path content =
  match Path.save ~create ?append path content with
  | () -> traceln "write %a -> ok" Path.pp path
  | exception ex -> raise ex

let try_mkdir path =
  traceln "mkdir %a -> ?" Path.pp path;
  match Path.mkdir path ~perm:0o700 with
  | () -> traceln "mkdir %a -> ok" Path.pp path
  | exception ex -> raise ex

let try_mkdirs ?exists_ok path =
  match Path.mkdirs ?exists_ok path ~perm:0o700 with
  | () -> traceln "mkdirs %a -> ok" Path.pp path
  | exception ex -> traceln "@[<h>%a@]" Eio.Exn.pp ex

let try_rename p1 p2 =
  match Path.rename p1 p2 with
  | () -> traceln "rename %a to %a -> ok" Path.pp p1 Path.pp p2
  | exception ex -> raise ex

let try_read_dir path =
  match Path.read_dir path with
  | names -> traceln "read_dir %a -> %a" Path.pp path Fmt.Dump.(list string) names
  | exception ex -> raise ex

let try_unlink path =
  match Path.unlink path with
  | () -> traceln "unlink %a -> ok" Path.pp path
  | exception ex -> raise ex

let try_rmdir path =
  match Path.rmdir path with
  | () -> traceln "rmdir %a -> ok" Path.pp path
  | exception ex -> raise ex

let with_temp_file path fn =
 Fun.protect (fun () -> fn path) ~finally:(fun () -> Eio.Path.unlink path)

(* Remove [paths] after [fn], ignoring any that have gone already. List a
   directory after its contents. *)
let with_cleanup paths fn =
  let rm path =
    try Unix.unlink path
    with Unix.Unix_error _ -> (try Unix.rmdir path with Unix.Unix_error _ -> ())
  in
  Fun.protect fn ~finally:(fun () -> List.iter rm paths)

(* A temporary file made by the stdlib, so that [fs] is given an absolute path. *)
let with_stdlib_temp_file prefix fn =
  let path = Filename.temp_file prefix "" in
  with_cleanup [path] (fun () -> fn path)

(* Making a symlink needs a privilege that older Windows withholds. *)
let with_symlinks fn =
  if Unix.has_symlink () then fn () else Alcotest.skip ()

(* Read and write without Eio, to see what really landed on disk. *)
let write_file path data = Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc data)
let read_file path = In_channel.with_open_bin path In_channel.input_all

(* Check that [fn] reports a missing path as [Not_found] without creating it.
   [fn] describes what it got instead, for the failure message. *)
let check_missing name fn =
  let path = Filename.temp_file name "" in
  Unix.unlink path;
  with_cleanup [path] @@ fun () ->
  (match fn path with
   | got -> Alcotest.failf "Expected Not_found, got %s" got
   | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ());
  Alcotest.(check bool) "file not created" false (Sys.file_exists path)

let chdir path =
  traceln "chdir %S" path;
  Unix.chdir path

let assert_kind path kind =
  Path.with_open_in path @@ fun file ->
  assert ((Eio.File.stat file).kind = kind)

let test_create_and_read env () =
  let cwd = Eio.Stdenv.cwd env in
  let data = "my-data" in
  with_temp_file (cwd / "test-file") @@ fun path ->
  Path.save ~create:(`Exclusive 0o666) path data;
  Alcotest.(check string) "same data" data (Path.load path)

(* An absolute Windows path replaces the directory part, as "/" does. *)
let test_absolute_join env () =
  let fs = Eio.Stdenv.fs env in
  let check p expected =
    let (_, got) = fs / "sub" / p in
    Alcotest.(check string) p expected got
  in
  check "C:\\foo" "C:\\foo";
  check "C:/foo" "C:/foo";
  check "C:foo" "C:foo";   (* drive-relative also replaces *)
  check "\\foo" "\\foo";
  check "\\\\server\\share" "\\\\server\\share";
  check "/foo" "/foo";
  check "rel" "sub\\rel"

(* Splitting a path and re-joining with (/) refers to the same location. *)
let test_split_join env () =
  let fs = Eio.Stdenv.fs env in
  let check p expected =
    match Path.split (fs / p) with
    | None -> Alcotest.failf "%s: no split" p
    | Some (dir, base) -> Alcotest.(check string) p expected (snd (dir / base))
  in
  check "C:\\a\\b" "C:\\a\\b";
  check "C:\\b" "C:\\b";
  check "C:x" "C:x";              (* drive-relative: no separator added *)
  check "\\\\srv\\share\\x" "\\\\srv\\share\\x";
  check "\\\\?\\C:\\a\\b" "\\\\?\\C:\\a\\b"   (* verbatim: keeps "\\" *)

let test_native env () =
  let cwd = Eio.Stdenv.cwd env in
  (* Lexical, so nothing needs to exist. *)
  Alcotest.(check string) "relative" ".\\foo" (Path.native_exn (cwd / "foo"));
  Alcotest.(check string) "parent" ".\\.." (Path.native_exn (cwd / ".."));
  Alcotest.(check string) "empty" "." (Path.native_exn cwd);
  Alcotest.(check string) "fs relative" ".\\foo" (Path.native_exn (Eio.Stdenv.fs env / "foo"));
  Alcotest.(check string) "absolute" "C:\\foo" (Path.native_exn (Eio.Stdenv.fs env / "C:\\foo"));
  (* A subtree records its directory as an absolute Win32 path. *)
  Path.mkdir (cwd / "native-sub") ~perm:0o700;
  Fun.protect ~finally:(fun () -> Path.rmdir (cwd / "native-sub")) @@ fun () ->
  Path.with_open_dir (cwd / "native-sub") @@ fun sub ->
  let p = Path.native_exn (sub / "foo.txt") in
  Alcotest.(check bool) "sub absolute" false (Filename.is_relative p);
  Alcotest.(check bool) "no NT prefix" false (String.starts_with ~prefix:"\\??\\" p);
  Alcotest.(check string) "sub basename" "foo.txt" (Filename.basename p)

let test_cwd_no_access_abs env () =
  let cwd = Eio.Stdenv.cwd env in
  let temp = Filename.temp_file "eio" "win" in
  try
    Path.save ~create:(`Exclusive 0o666) (cwd / temp) "my-data";
    failwith "Should have failed"
  with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let test_exclusive env () =
  let cwd = Eio.Stdenv.cwd env in
  with_temp_file (cwd / "test-file") @@ fun path ->
  Eio.traceln "fiest";
  Path.save ~create:(`Exclusive 0o666) path "first-write";
  Eio.traceln "next";
  try
    Path.save ~create:(`Exclusive 0o666) path "first-write";
    Eio.traceln "nope";
    failwith "Should have failed"
  with Eio.Io (Eio.Fs.E (Already_exists _), _) -> ()

let test_if_missing env () =
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  with_temp_file test_file @@ fun test_file ->
  Path.save ~create:(`If_missing 0o666) test_file "1st-write-original";
  Path.save ~create:(`If_missing 0o666) test_file "2nd-write";
  Alcotest.(check string) "same contents" "2nd-write-original" (Path.load test_file)

let test_trunc env () =
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  with_temp_file test_file @@ fun test_file ->
  Path.save ~create:(`Or_truncate 0o666) test_file "1st-write-original";
  Path.save ~create:(`Or_truncate 0o666) test_file "2nd-write";
  Alcotest.(check string) "same contents" "2nd-write" (Path.load test_file)

let test_empty env () =
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  try
    Path.save ~create:`Never test_file "1st-write-original";
    traceln "Got %S" @@ Path.load test_file;
    failwith "Should have failed"
  with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()

let test_append env () =
  let cwd = Eio.Stdenv.cwd env in
  let test_file = (cwd / "test-file") in
  with_temp_file test_file @@ fun test_file ->
  Path.save ~create:(`Or_truncate 0o666) test_file "1st-write-original";
  Path.save ~create:`Never ~append:true test_file "2nd-write";
  Alcotest.(check string) "append" "1st-write-original2nd-write" (Path.load test_file)

let test_mkdir env () =
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "subdir");
  try_mkdir (cwd / "subdir\\nested");
  let test_file = cwd / "subdir\\nested\\test-file" in
  Path.save ~create:(`Exclusive 0o600) test_file "data";
  Alcotest.(check string) "mkdir" "data" (Path.load test_file);
  Unix.unlink "subdir\\nested\\test-file";
  Unix.rmdir "subdir\\nested";
  Unix.rmdir "subdir"

let test_mkdirs env () =
  let cwd = Eio.Stdenv.cwd env in
  let nested = cwd / "subdir1" / "subdir2" / "subdir3" in
  try_mkdirs nested;
  let one_more = Path.(nested / "subdir4") in
  (try
    try_mkdirs one_more
  with Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  try_mkdirs ~exists_ok:true one_more;
  try
    try_mkdirs (cwd / ".." / "outside")
  with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let test_symlink env () =
  (*
    Important note: assuming that neither "another" nor
    "to-subdir" exist, the following program will behave
    differently if you don't have the ~to_dir flag.

    With [to_dir] set to [true] we get the desired UNIX behaviour,
    without it [Unix.realpath] will actually show the parent directory
    of "another". Presumably this is because Windows distinguishes
    between file symlinks and directory symlinks. Fun.

  {[ Unix.symlink ~to_dir:true "another" "to-subdir";
     Unix.mkdir "another" 0o700;
     print_endline @@ Unix.realpath "to-subdir" |}
  *)
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "sandbox");
  Unix.symlink ~to_dir:true ".." "sandbox\\to-root";
  Unix.symlink ~to_dir:true "subdir" "sandbox\\to-subdir";
  Unix.symlink ~to_dir:true "foo" "sandbox\\dangle";
  try_mkdir (cwd / "tmp");
  Eio.Path.with_subtree (cwd / "sandbox") @@ fun sandbox ->
  try_mkdir (sandbox / "subdir");
  try_mkdir (sandbox / "to-subdir\\nested");
  let () =
    try
      try_mkdir (sandbox / "to-root\\tmp\\foo");
      failwith "Expected permission denied to-root"
    with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()
  in
  assert (not (Sys.file_exists ".\\tmp\\foo"));
  let () =
    try
      try_mkdir (sandbox / "..\\foo");
      failwith "Expected permission denied parent foo"
    with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()
  in
  let () =
    try
      try_mkdir (sandbox / "to-subdir");
      failwith "Expected already exists"
    with Eio.Io (Eio.Fs.E (Already_exists _), _) -> ()
  in
  let () =
    try
      try_mkdir (sandbox / "dangle\\foo");
      failwith "Expected permission denied dangle foo"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
    ()

let test_unlink env () =
  let cwd = Eio.Stdenv.cwd env in
  Path.save ~create:(`Exclusive 0o600) (cwd / "file") "data";
  try_mkdir (cwd / "subdir");
  Path.save ~create:(`Exclusive 0o600) (cwd / "subdir\\file2") "data2";
  try_read_file (cwd / "file");
  try_read_file (cwd / "subdir\\file2");
  try_unlink (cwd / "file");
  try_unlink (cwd / "subdir\\file2");
  let () =
    try
      try_read_file (cwd / "file");
      failwith "file should not exist"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
  let () =
    try
      try_read_file (cwd / "subdir\\file2");
      failwith "file should not exist"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
  try_write_file ~create:(`Exclusive 0o600) (cwd / "subdir\\file2") "data2";
  (* Supposed to use symlinks here. *)
  try_unlink (cwd / "subdir\\file2");
  let () =
    try
      try_read_file (cwd / "subdir\\file2");
      failwith "file should not exist"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
  ()

let try_failing_unlink env () =
  let cwd = Eio.Stdenv.cwd env in
  let () =
    try
      try_unlink (cwd / "missing");
      failwith "Expected not found!"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
  let () =
    try
      try_unlink (cwd / "..\\foo");
      failwith "Expected permission denied!"
    with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()
  in
  ()

let test_remove_dir env () =
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "d1");
  try_mkdir (cwd / "subdir\\d2");
  try_read_dir (cwd / "d1");
  try_read_dir (cwd / "subdir\\d2");
  try_rmdir (cwd / "d1");
  try_rmdir (cwd / "subdir\\d2");
  let () =
    try
      try_read_dir (cwd / "d1");
      failwith "Expected not found"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
  let () =
    try
      try_read_dir (cwd / "subdir\\d2");
      failwith "Expected not found"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
  in
  ()

(* Absolute Win32 paths via the unsandboxed [fs] (#931) *)
let test_fs_absolute_read env () =
  let fs = Eio.Stdenv.fs env in
  with_stdlib_temp_file "eio-abs" @@ fun path ->
  let data = "abs-read-data" in
  write_file path data;
  Alcotest.(check string) "same data" data (Path.load (fs / path));
  let dir = Filename.dirname path in
  let unnormalised = String.concat "/" [dir; ".."; Filename.basename dir; Filename.basename path] in
  Alcotest.(check string) "with .. and /" data (Path.load (fs / unnormalised))

let test_fs_absolute_write env () =
  let fs = Eio.Stdenv.fs env in
  let path = Filename.temp_file "eio-abs-write" "" in
  Unix.unlink path;
  with_cleanup [path] @@ fun () ->
  let data = "abs-write-data" in
  Path.save ~create:(`Exclusive 0o600) (fs / path) data;
  Alcotest.(check string) "same data" data (read_file path)

let test_fs_absolute_unlink env () =
  let fs = Eio.Stdenv.fs env in
  with_stdlib_temp_file "eio-abs-unlink" @@ fun path ->
  Path.unlink (fs / path);
  Alcotest.(check bool) "file gone" false (Sys.file_exists path)

let test_fs_absolute_mkdir_rmdir env () =
  let fs = Eio.Stdenv.fs env in
  let path = Filename.temp_file "eio-abs-dir" "" in
  Unix.unlink path;
  with_cleanup [path] @@ fun () ->
  Path.mkdir ~perm:0o700 (fs / path);
  Alcotest.(check bool) "is dir" true (Sys.is_directory path);
  Path.rmdir (fs / path);
  Alcotest.(check bool) "dir gone" false (Sys.file_exists path)

let test_fs_relative_read env () =
  let fs = Eio.Stdenv.fs env in
  let name = "fs-rel-test-file" in
  let data = "rel-read-data" in
  with_cleanup [name] @@ fun () ->
  write_file name data;
  Alcotest.(check string) "same data" data (Path.load (fs / name))

let test_fs_nt_prefixed_read env () =
  let fs = Eio.Stdenv.fs env in
  with_stdlib_temp_file "eio-nt" @@ fun path ->
  let data = "nt-prefixed-data" in
  write_file path data;
  Alcotest.(check string) "same data" data (Path.load (fs / ("\\??\\" ^ path)))

let test_fs_symlink_follow_read env () =
  with_symlinks @@ fun () ->
  let fs = Eio.Stdenv.fs env in
  let data = "symlink-follow-data" in
  let target = "slt-target" and link = "slt-link" in
  with_cleanup [link; target] @@ fun () ->
  write_file target data;
  Unix.symlink target link;
  Alcotest.(check string) "relative link" data (Path.load (fs / link));
  let abs_link = Filename.concat (Sys.getcwd ()) link in
  Alcotest.(check string) "absolute link" data (Path.load (fs / abs_link))

let test_sandbox_write_through_symlink_leaf env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let target = "slt2-target" and link = "slt2-link" in
  with_cleanup [link; target] @@ fun () ->
  write_file target "old";
  Unix.symlink target link;
  Path.save ~create:`Never (cwd / link) "new";
  Alcotest.(check string) "wrote through symlink" "new" (read_file target)

(* As above, but in a subtree, whose [dir_path] is absolute, and with a relative link target *)
let test_subtree_write_through_symlink_leaf env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let dir = "slt3-dir" in
  let target = dir ^ "\\target" and link = dir ^ "\\link" in
  with_cleanup [link; target; dir] @@ fun () ->
  try_mkdir (cwd / dir);
  write_file target "old";
  Unix.symlink "target" link;
  Eio.Path.with_subtree (cwd / dir) @@ fun sub ->
  Path.save ~create:`Never (sub / "link") "new";
  Alcotest.(check string) "wrote through subtree symlink" "new" (read_file target)

let test_sandbox_symlink_escape_write env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let dir = "slt4-dir" and outside = "slt4-outside" in
  let escape = dir ^ "\\escape" in
  with_cleanup [escape; dir; outside] @@ fun () ->
  try_mkdir (cwd / dir);
  write_file outside "unchanged";
  Unix.symlink ("..\\" ^ outside) escape;
  (try
     Eio.Path.with_subtree (cwd / dir) @@ fun sub ->
     Path.save ~create:`Never (sub / "escape") "x";
     failwith "Expected permission denied"
   with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ());
  Alcotest.(check string) "outside file unchanged" "unchanged" (read_file outside)

let test_fs_missing_read_no_create env () =
  let fs = Eio.Stdenv.fs env in
  check_missing "eio-missing-read" @@ fun path ->
  Fmt.str "%S" (Path.load (fs / path))

let test_fs_missing_stat_no_create env () =
  let fs = Eio.Stdenv.fs env in
  check_missing "eio-missing-stat" @@ fun path ->
  Fmt.str "kind %a" Eio.File.Stat.pp_kind (Eio.Path.stat ~follow:true (fs / path)).kind

let tests env = [
  "create-write-read", `Quick, test_create_and_read env;
  "absolute-join", `Quick, test_absolute_join env;
  "split-join", `Quick, test_split_join env;
  "native", `Quick, test_native env;
  "cwd-abs-path", `Quick, test_cwd_no_access_abs env;
  "create-exclusive", `Quick, test_exclusive env;
  "create-if_missing", `Quick, test_if_missing env;
  "create-trunc", `Quick, test_trunc env;
  "create-empty", `Quick, test_empty env;
  "append", `Quick, test_append env;
  "mkdir", `Quick, test_mkdir env;
  "symlinks", `Quick, test_symlink env;
  "unlink", `Quick, test_unlink env;
  "failing-unlink", `Quick, try_failing_unlink env;
  "rmdir", `Quick, test_remove_dir env;
  "mkdirs", `Quick, test_mkdirs env;
  "fs-absolute-read", `Quick, test_fs_absolute_read env;
  "fs-absolute-write", `Quick, test_fs_absolute_write env;
  "fs-absolute-unlink", `Quick, test_fs_absolute_unlink env;
  "fs-absolute-mkdir-rmdir", `Quick, test_fs_absolute_mkdir_rmdir env;
  "fs-relative-read", `Quick, test_fs_relative_read env;
  "fs-nt-prefixed-read", `Quick, test_fs_nt_prefixed_read env;
  "fs-symlink-follow-read", `Quick, test_fs_symlink_follow_read env;
  "sandbox-write-through-symlink-leaf", `Quick, test_sandbox_write_through_symlink_leaf env;
  "subtree-write-through-symlink-leaf", `Quick, test_subtree_write_through_symlink_leaf env;
  "sandbox-symlink-escape-write", `Quick, test_sandbox_symlink_escape_write env;
  "fs-missing-read-no-create", `Quick, test_fs_missing_read_no_create env;
  "fs-missing-stat-no-create", `Quick, test_fs_missing_stat_no_create env;
]
