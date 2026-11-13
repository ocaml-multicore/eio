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

let stat_kind = Alcotest.testable Eio.File.Stat.pp_kind ( = )

let check_kind ~follow path expected =
  Alcotest.check stat_kind (Fmt.str "%a ~follow:%b" Path.pp path follow) expected
    (Eio.Path.stat ~follow path).kind

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
  let paths = ["sandbox\\to-root"; "sandbox\\to-subdir"; "sandbox\\dangle"; "sandbox\\subdir\\nested"; "sandbox\\subdir"; "sandbox"; "tmp"] in
  with_cleanup paths @@ fun () ->
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

let test_stat_directory env () =
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["stat-dir"] @@ fun () ->
  try_mkdir (cwd / "stat-dir");
  check_kind ~follow:true (cwd / "stat-dir") `Directory;
  check_kind ~follow:false (cwd / "stat-dir") `Directory

let test_stat_regular_file env () =
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  with_cleanup ["stat-file"] @@ fun () ->
  Path.save ~create:(`Exclusive 0o600) (cwd / "stat-file") "data";
  let abs = Filename.concat (Sys.getcwd ()) "stat-file" in
  check_kind ~follow:true (cwd / "stat-file") `Regular_file;
  check_kind ~follow:false (cwd / "stat-file") `Regular_file;
  check_kind ~follow:true (fs / abs) `Regular_file;
  check_kind ~follow:false (fs / abs) `Regular_file

let test_stat_symlink env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  let target = "statl-target" and link = "statl-link" and dangling = "statl-dangling" in
  with_cleanup [link; dangling; target] @@ fun () ->
  write_file target "data";
  Unix.symlink target link;
  Unix.symlink "statl-missing" dangling;
  let abs_link = Filename.concat (Sys.getcwd ()) link in
  check_kind ~follow:false (cwd / link) `Symbolic_link;
  check_kind ~follow:true (cwd / link) `Regular_file;
  check_kind ~follow:false (fs / abs_link) `Symbolic_link;
  check_kind ~follow:true (fs / abs_link) `Regular_file;
  check_kind ~follow:false (cwd / dangling) `Symbolic_link;
  (match Eio.Path.stat ~follow:true (cwd / dangling) with
   | st -> Alcotest.failf "Expected Not_found, got %a" Eio.File.Stat.pp_kind st.kind
   | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ())

let test_rename env () =
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  with_cleanup ["rn-a"; "rn-b"; "rn-c"; "rn-dir\\moved"; "rn-dir2\\moved"; "rn-dir"; "rn-dir2"; "..\\rn-escaped"] @@ fun () ->
  Path.save ~create:(`Exclusive 0o600) (cwd / "rn-a") "a";
  Path.rename (cwd / "rn-a") (cwd / "rn-b");
  Alcotest.(check string) "renamed" "a" (read_file "rn-b");
  Alcotest.(check bool) "old name gone" false (Sys.file_exists "rn-a");
  write_file "rn-c" "c";
  Path.rename (cwd / "rn-b") (cwd / "rn-c");
  Alcotest.(check string) "existing target replaced" "a" (read_file "rn-c");
  try_mkdir (cwd / "rn-dir");
  Path.rename (cwd / "rn-c") (cwd / "rn-dir" / "moved");
  Alcotest.(check string) "moved into a directory" "a" (read_file "rn-dir\\moved");
  Path.rename (cwd / "rn-dir") (cwd / "rn-dir2");
  Alcotest.(check bool) "directory renamed" true (Sys.is_directory "rn-dir2");
  let abs = Filename.concat (Sys.getcwd ()) in
  Path.rename (fs / abs "rn-dir2\\moved") (fs / abs "rn-a");
  Alcotest.(check string) "renamed through fs" "a" (read_file "rn-a");
  match Path.rename (cwd / "rn-a") (cwd / "..\\rn-escaped") with
  | () -> Alcotest.fail "Expected permission denied"
  | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let test_rename_over_dir env () =
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["rn-src\\inside"; "rn-src"; "rn-empty\\inside"; "rn-empty"; "rn-full\\inside"; "rn-full"] @@ fun () ->
  try_mkdir (cwd / "rn-src");
  write_file "rn-src\\inside" "x";
  try_mkdir (cwd / "rn-empty");
  Path.rename (cwd / "rn-src") (cwd / "rn-empty");
  Alcotest.(check string) "empty directory replaced" "x" (read_file "rn-empty\\inside");
  Alcotest.(check bool) "old name gone" false (Sys.file_exists "rn-src");
  try_mkdir (cwd / "rn-full");
  write_file "rn-full\\inside" "y";
  match Path.rename (cwd / "rn-empty") (cwd / "rn-full") with
  | () -> Alcotest.fail "Expected ENOTEMPTY"
  | exception Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.ENOTEMPTY, _, _)), _) ->
    Alcotest.(check string) "non-empty directory kept" "y" (read_file "rn-full\\inside");
    Alcotest.(check string) "source kept" "x" (read_file "rn-empty\\inside")

let test_open_no_follow env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["dir1\\link"; "dir1\\file"; "link1"; "dir1"] @@ fun () ->
  let dir1 = cwd / "dir1" in
  let link1 = cwd / "link1" in
  Path.mkdir dir1 ~perm:0o700;
  Path.symlink ~link_to:"dir1" link1;
  let file = dir1 / "file" in
  let link2 = link1 / "link" in
  Path.save file "data1" ~create:(`Exclusive 0o600);
  Path.symlink ~link_to:"file" (dir1 / "link");
  Path.save ~create:`Never file "data2";
  Path.save ~create:`Never (link1 / "file") "data3";
  Path.save ~create:`Never link2 "data4";
  Path.save ~follow:false ~create:`Never file "data2";
  Path.save ~follow:false ~create:`Never (link1 / "file") "data3";
  begin
    try Path.save ~follow:false ~create:`Never link2 "data4"; Alcotest.fail "Expected symlink error on save"
    with Eio.Io (Eio.Fs.E Symlink, _) -> ()
  end;
  let try_read_file ~follow path =
    Alcotest.(check string) (Fmt.str "read %a follow=%b" Eio.Path.pp path follow)
      "data3" (Eio.Path.load ~follow path)
  in
  try_read_file ~follow:true file;
  try_read_file ~follow:true (link1 / "file");
  try_read_file ~follow:true link2;
  try_read_file ~follow:false file;
  try_read_file ~follow:false (link1 / "file");
  begin
    try try_read_file ~follow:false link2; Alcotest.fail "Expected symlink error on load"
    with Eio.Io (Eio.Fs.E Symlink, _) -> ()
  end

let check_symlink_error name fn =
  match fn () with
  | _ -> Alcotest.failf "%s: expected a symlink error" name
  | exception Eio.Io (Eio.Fs.E Symlink, _) -> ()

let check_permission_denied name fn =
  match fn () with
  | _ -> Alcotest.failf "%s: expected permission denied" name
  | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let test_eio_symlink env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["esl-dir\\f"; "esl-file-link"; "esl-dir-link"; "esl-dangle"; "esl-unc"; "esl-dir"] @@ fun () ->
  try_mkdir (cwd / "esl-dir");
  write_file "esl-dir\\f" "data";
  Path.symlink ~link_to:"esl-dir/f" (cwd / "esl-file-link");
  Path.symlink ~link_to:"esl-dir" (cwd / "esl-dir-link");
  Path.symlink ~link_to:"esl-missing" (cwd / "esl-dangle");
  Alcotest.(check string) "read through file link" "data" (Path.load (cwd / "esl-file-link"));
  Alcotest.(check (list string)) "list through dir link" ["f"] (Path.read_dir (cwd / "esl-dir-link"));
  Alcotest.(check (array string)) "native dir link" [| "f" |] (Sys.readdir "esl-dir-link");
  Alcotest.(check string) "native file link" "data" (read_file "esl-file-link");
  Alcotest.(check string) "read_link dir" "esl-dir" (Path.read_link (cwd / "esl-dir-link"));
  Alcotest.(check string) "read_link native" "esl-dir\\f" (Unix.readlink "esl-file-link");
  Path.symlink ~link_to:"\\\\srv\\share\\x" (cwd / "esl-unc");
  Alcotest.(check string) "read_link UNC" "\\\\srv\\share\\x" (Path.read_link (cwd / "esl-unc"));
  check_kind ~follow:false (cwd / "esl-dir-link") `Symbolic_link;
  check_kind ~follow:true (cwd / "esl-dir-link") `Directory;
  (match Path.symlink ~link_to:"esl-dir" (cwd / "esl-dir-link") with
   | () -> Alcotest.fail "Expected Already_exists"
   | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  with_cleanup ["esl-missing"] @@ fun () ->
  Path.save ~create:(`If_missing 0o600) (cwd / "esl-dangle") "new";
  Alcotest.(check string) "created target" "new" (read_file "esl-missing")

let test_symlink_encoding env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let target = "encoding-é-🦋" in
  let links = ["encoding-relative"; "encoding-absolute"; "encoding-limit"; "encoding-invalid"] in
  with_cleanup (links @ [target]) @@ fun () ->
  write_file target "unicode";
  List.iter (fun (name, link_to) ->
      Path.symlink ~link_to (cwd / name);
      Alcotest.(check string) "native reads encoded target" "unicode" (read_file name);
      Alcotest.(check string) "decoded target matches native" (Eio_utils.Nt_path.to_win32 (Unix.readlink name)) (Path.read_link (cwd / name));
      Alcotest.(check string) "sandbox reads encoded target" "unicode" (Path.load (cwd / name))
    ) ["encoding-relative", target;
       "encoding-absolute", "\\\\?\\" ^ Sys.getcwd () ^ "\\" ^ target];
  Alcotest.(check string) "native Unicode readlink" target (Unix.readlink "encoding-relative");
  Alcotest.(check string) "Eio Unicode readlink" target (Path.read_link (cwd / "encoding-relative"));
  (* 13 UTF-16 code units, including a surrogate pair. *)
  let size = (Path.stat ~follow:false (cwd / "encoding-relative")).size in
  Alcotest.(check int) "link size in UTF-16 bytes" 26 (Optint.Int63.to_int size);
  (* Header (20 bytes) plus two UTF-16 copies of the target fills 16 KiB. *)
  let longest = String.make 4091 'x' in
  Path.symlink ~link_to:longest (cwd / "encoding-limit");
  Alcotest.(check string) "maximum-sized reparse buffer" longest (Unix.readlink "encoding-limit");
  Alcotest.(check string) "decode maximum-sized buffer" longest (Path.read_link (cwd / "encoding-limit"));
  List.iter (fun link_to ->
      (match Path.symlink ~link_to (cwd / "encoding-invalid") with
       | () -> Alcotest.fail "expected invalid target to fail"
       | exception Eio.Io _ -> ());
      match Unix.lstat "encoding-invalid" with
      | _ -> Alcotest.fail "failed symlink creation left an object behind"
      | exception Unix.Unix_error (ENOENT, _, _) -> ()
    ) [""; "target\000ignored"; String.make 4092 'x']

let test_unlink_symlink env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["usl-file-link"; "usl-dir-link"; "usl-dir-link2"; "usl-file"; "usl-dir"] @@ fun () ->
  write_file "usl-file" "data";
  try_mkdir (cwd / "usl-dir");
  Unix.symlink "usl-file" "usl-file-link";
  Path.symlink ~link_to:"usl-dir" (cwd / "usl-dir-link");
  Path.symlink ~link_to:"usl-dir" (cwd / "usl-dir-link2");
  Path.unlink (cwd / "usl-file-link");
  Path.unlink (cwd / "usl-dir-link");
  Path.rmdir (cwd / "usl-dir-link2");
  Alcotest.(check bool) "file link gone" false (Sys.file_exists "usl-file-link");
  Alcotest.(check bool) "dir link gone" false (Sys.file_exists "usl-dir-link");
  Alcotest.(check bool) "dir link gone (rmdir)" false (Sys.file_exists "usl-dir-link2");
  Alcotest.(check string) "file target kept" "data" (read_file "usl-file");
  Alcotest.(check bool) "dir target kept" true (Sys.is_directory "usl-dir");
  match Path.unlink (cwd / "usl-dir") with
  | () -> Alcotest.fail "unlink removed a directory"
  | exception Eio.Io _ -> ()

let test_subtree_symlinks env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let paths = ["sts\\a\\b\\f"; "sts\\a\\b"; "sts\\a\\up"; "sts\\a"; "sts\\to-b"; "sts\\abs"; "sts\\abs-in"; "sts\\loop1"; "sts\\loop2"; "sts\\f"; "sts\\out"; "sts"] in
  with_cleanup paths @@ fun () ->
  try_mkdir (cwd / "sts");
  Path.with_open_dir (cwd / "sts") @@ fun sub ->
  Path.mkdirs ~perm:0o700 (sub / "a" / "b");
  Path.save ~create:(`Exclusive 0o600) (sub / "a/b/f") "inner";
  Path.save ~create:(`Exclusive 0o600) (sub / "f") "outer";
  Path.symlink ~link_to:"a\\b" (sub / "to-b");
  Path.symlink ~link_to:".." (sub / "a" / "up");
  Path.symlink ~link_to:"..\\.." (sub / "out");
  Path.symlink ~link_to:(Sys.getcwd ()) (sub / "abs");
  Path.symlink ~link_to:(Filename.concat (Sys.getcwd ()) "sts\\a\\b") (sub / "abs-in");
  Path.symlink ~link_to:"loop2" (sub / "loop1");
  Path.symlink ~link_to:"loop1" (sub / "loop2");
  Alcotest.(check string) "via link" "inner" (Path.load (sub / "to-b" / "f"));
  Alcotest.(check string) "via absolute link inside" "inner" (Path.load (sub / "abs-in" / "f"));
  Alcotest.(check string) "via link to parent" "outer" (Path.load (sub / "a" / "up" / "f"));
  Alcotest.(check string) "read_link" "a\\b" (Path.read_link (sub / "to-b"));
  (* As in Win32, ".." applies to the path as written, before following links *)
  Alcotest.(check string) "lexical .." "outer" (Path.load (sub / "to-b" / ".." / "f"));
  check_permission_denied "link outside" (fun () -> Path.read_dir (sub / "out"));
  check_permission_denied "absolute link" (fun () -> Path.read_dir (sub / "abs"));
  check_permission_denied "write via link outside" (fun () ->
      Path.save ~create:(`Exclusive 0o600) (sub / "out" / "sts-escaped") "x");
  check_permission_denied "symlink via link outside" (fun () ->
      Path.symlink ~link_to:"x" (sub / "out" / "sts-escaped"));
  Alcotest.(check bool) "nothing escaped" false (Sys.file_exists "..\\sts-escaped");
  check_symlink_error "loop" (fun () -> Path.load (sub / "loop1"));
  check_symlink_error "no follow" (fun () -> Path.load ~follow:false (sub / "to-b"));
  Alcotest.(check string) "open_dir via link" "inner" (Path.with_open_dir (sub / "to-b") (fun b -> Path.load (b / "f")))

(* Opening a link without following it must fail before truncating anything. *)
let test_no_follow_truncate env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  with_cleanup ["nft-link"; "nft-target"] @@ fun () ->
  write_file "nft-target" "data";
  Path.symlink ~link_to:"nft-target" (cwd / "nft-link");
  List.iter (fun dir ->
      check_symlink_error "truncate" (fun () ->
          Path.save ~follow:false ~create:(`Or_truncate 0o600) (dir / "nft-link") "new");
      Alcotest.(check string) "link kept" "nft-target" (Path.read_link (dir / "nft-link"));
      Alcotest.(check string) "target kept" "data" (read_file "nft-target")
    ) [cwd; fs]

let test_junction env () =
  let cwd = Eio.Stdenv.cwd env in
  let paths = ["jn-dir\\f"; "jn-sub\\inner\\g"; "jn-sub\\out"; "jn-sub\\in"; "jn-sub\\inner"; "jn-sub"; "jn-dir"] in
  with_cleanup paths @@ fun () ->
  try_mkdir (cwd / "jn-dir");
  write_file "jn-dir\\f" "data";
  Path.mkdirs ~perm:0o700 (cwd / "jn-sub" / "inner");
  write_file "jn-sub\\inner\\g" "inner";
  if Sys.command "mklink /J jn-sub\\out jn-dir > NUL && mklink /J jn-sub\\in jn-sub\\inner > NUL" <> 0 then
    Alcotest.fail "mklink /J failed";
  check_kind ~follow:false (cwd / "jn-sub" / "out") `Symbolic_link;
  check_kind ~follow:true (cwd / "jn-sub" / "out") `Directory;
  Alcotest.(check string) "read_link" (Filename.concat (Sys.getcwd ()) "jn-dir") (Path.read_link (cwd / "jn-sub" / "out"));
  Alcotest.(check string) "within cwd" "data" (Path.load (cwd / "jn-sub" / "out" / "f"));
  Path.with_open_dir (cwd / "jn-sub") (fun sub ->
      Alcotest.(check string) "within subtree" "inner" (Path.load (sub / "in" / "g"));
      Alcotest.(check (list string)) "list via junction" ["g"] (Path.read_dir (sub / "in"));
      check_permission_denied "outside subtree" (fun () -> Path.load (sub / "out" / "f"))
    );
  Path.rmtree (cwd / "jn-sub");
  Alcotest.(check bool) "tree gone" false (Sys.file_exists "jn-sub");
  Alcotest.(check string) "target kept" "data" (read_file "jn-dir\\f")

(* Exclusive creation must reject the link itself, including a dangling link. *)
let test_exclusive_symlink env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env and fs = Eio.Stdenv.fs env in
  with_cleanup ["exc-dangle"; "exc-link"; "exc-dir-link"; "exc-missing";
                "exc-target"; "exc-dir\\created-cwd"; "exc-dir\\created-fs"; "exc-dir"] @@ fun () ->
  write_file "exc-target" "original";
  Unix.mkdir "exc-dir" 0o700;
  Unix.symlink "exc-missing" "exc-dangle";
  Unix.symlink "exc-target" "exc-link";
  Unix.symlink ~to_dir:true "exc-dir" "exc-dir-link";
  List.iter (fun (dir, suffix) ->
      List.iter (fun follow ->
          List.iter (fun name ->
              match Path.save ~follow ~create:(`Exclusive 0o600) (dir / name) "bad" with
              | () -> Alcotest.failf "exclusive creation followed %s" name
              | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ()
            ) ["exc-dangle"; "exc-link"]
        ) [true; false];
      Path.save ~create:(`Exclusive 0o600) (dir / "exc-dir-link" / ("created-" ^ suffix)) "ok"
    ) [cwd, "cwd"; fs, "fs"];
  Alcotest.(check bool) "dangling target not created" false (Sys.file_exists "exc-missing");
  Alcotest.(check string) "existing target unchanged" "original" (read_file "exc-target");
  Alcotest.(check string) "created via parent link" "ok" (read_file "exc-dir\\created-cwd")

let test_absolute_directory_symlink env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env and fs = Eio.Stdenv.fs env in
  with_cleanup ["ads\\dir\\f"; "ads\\abs"; "ads\\relative"; "ads\\verbatim"; "ads\\rooted"; "ads\\drive-relative";
                "ads\\dir"; "ads"] @@ fun () ->
  Unix.mkdir "ads" 0o700;
  Unix.mkdir "ads\\dir" 0o700;
  write_file "ads\\dir\\f" "data";
  Path.with_open_dir (cwd / "ads") @@ fun sub ->
  let target = Filename.concat (Sys.getcwd ()) "ads\\dir" in
  List.iter (fun (name, link_to) ->
      Path.symlink ~link_to (sub / name);
      Alcotest.(check (array string)) "native directory link" [|"f"|] (Sys.readdir ("ads\\" ^ name));
      List.iter (fun dir ->
          Alcotest.(check (list string)) "Eio directory link" ["f"] (Path.read_dir (dir / "ads" / name))
        ) [cwd; fs]
    ) ["abs", target; "relative", "dir"; "verbatim", "\\\\?\\" ^ target;
       "rooted", String.sub target 2 (String.length target - 2);
       "drive-relative", String.sub target 0 2 ^ "ads\\dir"]

let test_symlink_parent env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env and fs = Eio.Stdenv.fs env in
  let links = ["cwd"; "fs"; "sub"] in
  with_cleanup (List.concat_map (fun name -> ["slp\\a\\b\\" ^ name; "slp\\a\\b\\" ^ name ^ "-file"]) links @
                ["slp\\alias"; "slp\\a\\dir\\keep"; "slp\\a\\file"; "slp\\file";
                 "slp\\a\\b"; "slp\\a\\dir"; "slp\\a"; "slp"]) @@ fun () ->
  List.iter (fun p -> Unix.mkdir p 0o700) ["slp"; "slp\\a"; "slp\\a\\b"; "slp\\a\\dir"; "slp\\file"];
  write_file "slp\\a\\dir\\keep" "data";
  write_file "slp\\a\\file" "file data";
  Path.symlink ~link_to:"a\\b" (cwd / "slp" / "alias");
  Path.with_open_dir (cwd / "slp") @@ fun sub ->
  List.iter (fun (dir, name) ->
      Path.symlink ~link_to:"..\\dir" (dir / "alias" / name);
      Path.symlink ~link_to:"..\\file" (dir / "alias" / (name ^ "-file"));
      let native = "slp\\a\\b\\" ^ name in
      Alcotest.(check (array string)) "native directory link" [|"keep"|] (Sys.readdir native);
      Alcotest.(check string) "native file link" "file data" (read_file (native ^ "-file"));
      Alcotest.(check (list string)) "Eio directory link" ["keep"] (Path.read_dir (dir / "alias" / name));
      Alcotest.(check string) "relative target preserved" "..\\dir" (Unix.readlink native)
    ) [cwd / "slp", "cwd"; fs / "slp", "fs"; sub, "sub"]

let test_unlink_delete_access env () =
  let fs = Eio.Stdenv.fs env in
  let paths = ["uda\\file"; "uda\\control"; "uda\\dir"] in
  with_cleanup (paths @ ["uda"]) @@ fun () ->
  Unix.mkdir "uda" 0o700;
  Unix.mkdir "uda\\dir" 0o700;
  List.iter (fun p -> write_file p "data") ["uda\\file"; "uda\\control"];
  let icacls path args = Sys.command ("icacls " ^ path ^ " " ^ args ^ " >nul 2>&1") in
  Fun.protect ~finally:(fun () ->
      List.iter (fun p -> ignore (icacls p "/remove:d *S-1-1-0")) ("uda" :: paths)) @@ fun () ->
  List.iter (fun p ->
      Alcotest.(check int) "deny read attributes" 0 (icacls p "/deny \"*S-1-1-0:(RA)\"")) paths;
  (* Parent listing can otherwise grant access to the child's attributes. *)
  Alcotest.(check int) "deny parent listing" 0 (icacls "uda" "/deny \"*S-1-1-0:(RD)\"");
  Unix.unlink "uda\\control";  (* Native deletion works with these permissions. *)
  Path.unlink (fs / "uda" / "file");
  (match Path.unlink (fs / "uda" / "dir") with
   | () -> Alcotest.fail "unlink removed an ordinary directory"
   | exception Eio.Io _ -> ());
  Path.rmdir (fs / "uda" / "dir");
  List.iter (fun p ->
      match Unix.lstat p with
      | _ -> Alcotest.failf "%s was not deleted" p
      | exception Unix.Unix_error (ENOENT, _, _) -> ()) paths

external set_junction : string -> string option -> unit = "caml_test_set_junction"

let test_cwd_reparse env () =
  let cwd = Eio.Stdenv.cwd env in
  let base = Sys.getcwd () in
  let inside = Filename.concat base "cwd-inside" in
  let outside = Filename.concat base "cwd-outside" in
  with_cleanup ["cwd-outside\\secret"; "cwd-outside\\created"; "cwd-inside"; "cwd-outside"] @@ fun () ->
  Unix.mkdir inside 0o700;
  Unix.mkdir outside 0o700;
  write_file (Filename.concat outside "secret") "outside";
  Fun.protect ~finally:(fun () -> Unix.chdir base) @@ fun () ->
  Unix.chdir outside;
  Alcotest.(check string) "cwd follows chdir" "outside" (Path.load (cwd / "secret"));
  Unix.chdir inside;
  Alcotest.(check (list string)) "cwd changed back" [] (Path.read_dir cwd);
  set_junction inside (Some outside);
  Fun.protect ~finally:(fun () -> set_junction inside None) @@ fun () ->
  let denied fn =
    match fn () with
    | _ -> Alcotest.fail "cwd followed a replaced root"
    | exception Eio.Io _ -> ()
  in
  denied (fun () -> Path.read_dir cwd);
  denied (fun () -> Path.load (cwd / "secret"));
  denied (fun () -> Path.symlink ~link_to:"missing" (cwd / "created"));
  match Unix.lstat (Filename.concat outside "created") with
  | _ -> Alcotest.fail "symlink created outside cwd"
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

let test_unlink_handle env () =
  let module L = Eio_windows.Low_level in
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["uh-link"; "uh-target\\keep"; "uh-target"] @@ fun () ->
  Unix.mkdir "uh-target" 0o700;
  Unix.mkdir "uh-link" 0o700;
  set_junction "uh-link" (Some (Filename.concat (Sys.getcwd ()) "uh-target"));
  write_file "uh-target\\keep" "kept";
  (* Directory links must be checked and deleted using the same handle. *)
  L.unlink ~dir:false "uh-link";
  Alcotest.(check bool) "junction removed" false (Sys.file_exists "uh-link");
  Alcotest.(check string) "junction target kept" "kept" (read_file "uh-target\\keep");
  Unix.mkdir "uh-link" 0o700;
  (match L.unlink ~dir:false "uh-link" with
   | () -> Alcotest.fail "native unlink removed an ordinary directory"
   | exception Unix.Unix_error _ -> ());
  (match Path.unlink (cwd / "uh-link") with
   | () -> Alcotest.fail "unlink removed an ordinary directory"
   | exception Eio.Io _ -> ());
  Alcotest.(check bool) "ordinary directory kept" true (Sys.is_directory "uh-link")

(* Unlike rename, installing reparse data is allowed while a directory is open. *)
let with_retargeted_subtree env fn =
  let cwd = Eio.Stdenv.cwd env in
  with_cleanup ["rp-inside\\direct"; "rp-outside\\direct"; "rp-inside\\created"; "rp-outside\\created"; "rp-outside\\secret";
                "rp-inside"; "rp-outside"] @@ fun () ->
  Unix.mkdir "rp-inside" 0o700;
  Unix.mkdir "rp-outside" 0o700;
  write_file "rp-outside\\secret" "outside";
  Path.with_open_dir (cwd / "rp-inside") @@ fun sub ->
  Switch.run @@ fun sw ->
  let module L = Eio_windows.Low_level in
  let parent = Some (L.openat ~sw "rp-inside" L.Flags.Open.(generic_read + synchronise) L.Flags.Disposition.open_ L.Flags.Create.directory) in
  set_junction "rp-inside" (Some (Filename.concat (Sys.getcwd ()) "rp-outside"));
  Fun.protect ~finally:(fun () -> set_junction "rp-inside" None) (fun () -> fn sub parent)

let test_subtree_reparse_read env () =
  with_retargeted_subtree env @@ fun sub _parent ->
  Alcotest.(check (array string)) "path now points outside" [|"secret"|] (Sys.readdir "rp-inside");
  (* Rejecting a changed root is also safe; following it outside is not. *)
  (match Path.read_dir sub with
   | names -> Alcotest.(check (list string)) "open subtree stays inside" [] names
   | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ());
  match Path.load (sub / "secret") with
  | _ -> Alcotest.fail "read escaped through a replaced sandbox root"
  | exception Eio.Io (Eio.Fs.E (Not_found _ | Permission_denied _), _) -> ()

let test_subtree_reparse_symlink env () =
  with_symlinks @@ fun () ->
  with_retargeted_subtree env @@ fun sub parent ->
  (match Path.symlink ~link_to:"missing" (sub / "created") with
   | () -> ()
   | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ());
  (* Reproduce the change after parent lookup, just before the native operation. *)
  (try Eio_windows.Low_level.symlink ~link_to:"missing" parent "direct"
   with Unix.Unix_error _ -> ());  (* Windows may reject a changed parent. *)
  (match Unix.lstat "rp-outside\\direct" with
   | _ -> Alcotest.fail "handle-relative symlink creation escaped"
   | exception Unix.Unix_error (ENOENT, _, _) -> ());
  match Unix.lstat "rp-outside\\created" with
  | _ -> Alcotest.fail "symlink creation escaped through a replaced sandbox root"
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

let test_readdir_handle _env () =
  let module L = Eio_windows.Low_level in
  let names = List.init 512 (fun i -> Printf.sprintf "é-🦋-%04d-%s" i (String.make 80 'x')) in
  with_cleanup (List.map (fun n -> "rd-handle\\" ^ n) names @ ["rd-outside\\secret"; "rd-handle"; "rd-outside"]) @@ fun () ->
  Unix.mkdir "rd-handle" 0o700;
  Unix.mkdir "rd-outside" 0o700;
  write_file "rd-outside\\secret" "outside";
  Switch.run @@ fun sw ->
  let fd = L.openat ~sw "rd-handle" L.Flags.Open.(generic_read + synchronise) L.Flags.Disposition.open_ L.Flags.Create.directory in
  set_junction "rd-handle" (Some (Filename.concat (Sys.getcwd ()) "rd-outside"));
  Fun.protect ~finally:(fun () -> set_junction "rd-handle" None) (fun () ->
      Alcotest.(check (array string)) "enumerate handle, not junction target" [||] (L.readdir_fd fd));
  List.iter (fun n -> write_file ("rd-handle\\" ^ n) "") names;
  (* More than one native buffer, non-ASCII names, and restarting the same handle. *)
  for _ = 1 to 2 do
    List.iter (fun read ->
        let actual = read () in
        Array.sort String.compare actual;
        Alcotest.(check (array string)) "all names" (Array.of_list names) actual
      ) [(fun () -> L.readdir_fd fd); (fun () -> L.readdir "rd-handle")]
  done

let test_literal_link_targets env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let base = "\\\\?\\" ^ Sys.getcwd () ^ "\\literal" in
  let files = ["file"; "file."; "file "; "dir.\\keep"; "dir \\keep"; "...\\keep"] in
  let links = ["dot"; "space"; "absolute"; "rooted"; "dir-dot"; "dir-space"; "dot-component"; "cancel-dots"; "escape"; "back"] in
  with_cleanup (List.map (fun p -> base ^ "\\" ^ p) (links @ files @ ["dir."; "dir "; "..."]) @
                [base; "literal-outside"]) @@ fun () ->
  Unix.mkdir base 0o700;
  List.iter (fun p -> Unix.mkdir (base ^ "\\" ^ p) 0o700) ["dir."; "dir "; "..."];
  List.iter (fun p -> write_file (base ^ "\\" ^ p) "literal") files;
  write_file (base ^ "\\file") "plain";
  write_file "literal-outside" "outside";
  Path.with_open_dir (cwd / "literal") @@ fun sub ->
  let rooted = String.sub (Sys.getcwd ()) 2 (String.length (Sys.getcwd ()) - 2) ^ "\\literal\\file." in
  List.iter (fun (name, target, actual) ->
      Unix.symlink target (base ^ "\\" ^ name);
      write_file (base ^ "\\" ^ actual) "literal";
      Alcotest.(check string) "native reads literal target" "literal" (read_file (base ^ "\\" ^ name));
      Alcotest.(check string) "sandbox reads same target" "literal" (Path.load (sub / name));
      Path.save ~create:(`Or_truncate 0o600) (sub / name) "changed";
      Alcotest.(check string) "literal target updated" "changed" (read_file (base ^ "\\" ^ actual));
      Alcotest.(check string) "plain sibling unchanged" "plain" (read_file (base ^ "\\file"))
    ) ["dot", "file.", "file."; "space", "file ", "file ";
       "absolute", base ^ "\\file.", "file."; "rooted", rooted, "file."];
  Alcotest.(check string) "relative caller names are literal" "changed" (Path.load (sub / "file."));
  Alcotest.(check string) "unrestricted paths use Win32 rules" "plain"
    (Path.load (Eio.Stdenv.fs env / (Sys.getcwd () ^ "\\literal\\file.")));
  List.iter (fun (name, target) ->
      Path.symlink ~link_to:target (sub / name);
      Alcotest.(check (array string)) "native directory link" [|"keep"|] (Sys.readdir (base ^ "\\" ^ name));
      Alcotest.(check string) "literal intermediate component" "literal" (Path.load (sub / name / "keep"))
    ) ["dir-dot", "dir."; "dir-space", "dir "];
  List.iter (fun (name, target, expected) ->
      Path.symlink ~link_to:target (sub / name);
      Alcotest.(check string) "native dot component" expected (read_file (base ^ "\\" ^ name));
      Alcotest.(check string) "sandbox dot component" expected (Path.load (sub / name))
    ) ["dot-component", "...\\keep", "literal"; "cancel-dots", "...\\..\\file", "plain"];
  List.iter (fun (name, target) ->
      Unix.symlink target (base ^ "\\" ^ name);
      match Path.load (sub / name) with
      | _ -> Alcotest.fail "literal target escaped its sandbox"
      | exception Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()
    ) ["escape", "..\\literal-outside"; "back", "..\\literal\\file."]

let test_literal_entries env () =
  let cwd = Eio.Stdenv.cwd env in
  let base = "\\\\?\\" ^ Sys.getcwd () ^ "\\literal-entries" in
  let names = ["victim."; "victim "] in
  with_cleanup (List.concat_map (fun n -> [base ^ "\\" ^ n ^ "\\child."; base ^ "\\" ^ n]) names @
                [base ^ "\\victim"; base]) @@ fun () ->
  Unix.mkdir base 0o700;
  write_file (base ^ "\\victim") "keep";
  List.iter (fun n ->
      Unix.mkdir (base ^ "\\" ^ n) 0o700;
      write_file (base ^ "\\" ^ n ^ "\\child.") "child"
    ) names;
  let fs_entries = Path.read_dir_entries (Eio.Stdenv.fs env / "literal-entries") in
  List.iter (fun name ->
      Alcotest.(check bool) "unrestricted listing preserves literal names" true
        (List.mem (`Directory, name) fs_entries)
    ) names;
  Path.with_open_dir (cwd / "literal-entries") @@ fun sub ->
  let entries = Path.read_dir_entries sub in
  List.iter (fun name ->
      Alcotest.(check bool) "listed entry is a directory" true
        (List.mem (`Directory, name) entries);
      let listed = List.find ((=) name) (Path.read_dir sub) in
      Path.rmtree (sub / listed);
      Alcotest.(check bool) "literal entry removed" false (Sys.file_exists (base ^ "\\" ^ name));
      Alcotest.(check string) "plain sibling preserved" "keep" (Path.load (sub / "victim"))
    ) names

let test_invalid_link_targets env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let base = "\\\\?\\" ^ Sys.getcwd () ^ "\\invalid-targets" in
  let dot_names = ["..."; "...."; "... "; ".. "; ". "; " . "] in
  let targets = [base ^ "\\dir\\..\\file"; base ^ "\\.\\file";
                 base ^ "\\missing\\..\\file"] @ dot_names in
  let names = List.mapi (fun i _ -> Printf.sprintf "link-%d" i) targets in
  with_cleanup (List.map (fun n -> base ^ "\\" ^ n) (names @ dot_names @ ["file"; "dir"]) @ [base]) @@ fun () ->
  Unix.mkdir base 0o700;
  Unix.mkdir (base ^ "\\dir") 0o700;
  write_file (base ^ "\\file") "keep";
  List.iter (fun n -> write_file (base ^ "\\" ^ n) "literal dots") dot_names;
  Path.with_open_dir (cwd / "invalid-targets") @@ fun sub ->
  List.iter2 (fun name target ->
      Unix.symlink target (base ^ "\\" ^ name);
      (match read_file (base ^ "\\" ^ name) with
       | _ -> Alcotest.fail "native unexpectedly followed invalid target"
       | exception Sys_error _ -> ());
      (match Path.load (sub / name) with
       | _ -> Alcotest.fail "sandbox followed invalid target"
       | exception Eio.Io _ -> ());
      (match Path.save ~create:(`Or_truncate 0o600) (sub / name) "wrong" with
       | () -> Alcotest.fail "sandbox wrote through invalid target"
       | exception Eio.Io _ -> ())
    ) names targets;
  Alcotest.(check string) "plain file unchanged" "keep" (Path.load (sub / "file"));
  Alcotest.(check string) "literal dots accessible directly" "literal dots" (Path.load (sub / "..."))

let test_literal_symlink_parent env () =
  with_symlinks @@ fun () ->
  let fs = Eio.Stdenv.fs env in
  List.iter (fun name ->
      let parent = "\\\\?\\" ^ Sys.getcwd () ^ "\\" ^ name in
      with_cleanup (List.map (fun p -> parent ^ "\\" ^ p) ["link"; "target\\keep"; "target"] @ [parent]) @@ fun () ->
      Unix.mkdir parent 0o700;
      Path.with_open_dir (fs / parent) @@ fun sub ->
      Path.mkdir ~perm:0o700 (sub / "target");
      Path.save ~create:(`Exclusive 0o600) (sub / "target" / "keep") "data";
      Path.symlink ~link_to:"target" (sub / "link");
      Alcotest.(check (array string)) "native directory link" [|"keep"|] (Sys.readdir (parent ^ "\\link"));
      Alcotest.(check string) "sandbox directory link" "data" (Path.load (sub / "link" / "keep"))
    ) ["parent."; "parent "]

let test_trailing_link ~junction env () =
  with_symlinks @@ fun () ->
  let cwd = Eio.Stdenv.cwd env in
  let base = "trailing-link" in
  let native p = base ^ "\\" ^ p in
  let links = ["ok"; "up"; "up2"; "up3"; "out"; "renamed"; "drop"] in
  with_cleanup (List.map (fun p -> native ("jail\\" ^ p)) links @
                List.map native ["jail\\sub\\keep"; "jail\\sub\\new"; "outside\\secret";
                                 "jail\\sub"; "jail"; "outside"; ""] ) @@ fun () ->
  List.iter (fun p -> Unix.mkdir (native p) 0o700) [""; "jail"; "jail\\sub"; "outside"];
  write_file (native "outside\\secret") "secret";
  write_file (native "jail\\sub\\keep") "inside";
  let link name target =
    let path = native ("jail\\" ^ name) in
    if junction then (
      Unix.mkdir path 0o700;
      set_junction path (Some (Eio_utils.Nt_path.to_win32
          (Eio_utils.Nt_path.to_nt ~cwd:(Sys.getcwd () ^ "\\" ^ native "jail") target)))
    ) else Unix.symlink ~to_dir:true target path
  in
  List.iter (fun (name, target) -> link name target)
    ["ok", "sub"; "up", ".."; "up2", "up/"; "up3", "up\\"; "out", "..\\outside"];
  Path.with_open_dir (cwd / base / "jail") @@ fun jail ->
  let denied name fn = check_permission_denied name fn in
  List.iter (fun suffix ->
      Alcotest.(check (list string)) "safe directory link" ["keep"] (Path.read_dir (jail / ("ok" ^ suffix)));
      Path.mkdir ~perm:0o700 (jail / ("sub/new" ^ suffix));
      Path.rmdir (jail / ("sub/new" ^ suffix));
      List.iter (fun name ->
          let path = jail / (name ^ suffix) in
          denied "read_dir" (fun () -> Path.read_dir path);
          denied "read_dir_entries" (fun () -> Path.read_dir_entries path);
          denied "stat target" (fun () -> Path.stat ~follow:true path);
          denied "open directory" (fun () -> Path.with_open_dir path (fun _ -> ()));
          denied "open file" (fun () -> Path.load path);
          denied "read child" (fun () -> Path.load (path / "secret"));
          denied "truncate child" (fun () -> Path.save ~create:(`Or_truncate 0o600) (path / "secret") "wrong");
          denied "mkdir child" (fun () -> Path.mkdir ~perm:0o700 (path / "created"));
          denied "symlink child" (fun () -> Path.symlink ~link_to:"missing" (path / "created"))
        ) ["up"; "up2"; "up3"; "out"];
      let entry = jail / ("out" ^ suffix) in
      check_kind ~follow:false entry `Symbolic_link;
      Alcotest.(check string) "read link entry" (Path.read_link (jail / "out")) (Path.read_link entry);
      (match Path.mkdir ~perm:0o700 entry with
       | () -> Alcotest.fail "mkdir replaced a link"
       | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
      List.iter (fun follow ->
          List.iter (fun fn ->
              match fn () with
              | () -> Alcotest.fail "sandbox permission operation unexpectedly succeeded"
              | exception Eio.Io _ -> ())
            [(fun () -> Path.chmod ~follow ~perm:0o700 entry);
             (fun () -> Path.chown ~follow ~uid:0L entry)]
        ) [false; true];
      Path.rename entry (jail / ("renamed" ^ suffix));
      Path.rename (jail / ("renamed" ^ suffix)) (jail / "out");
      List.iter (fun remove ->
          link "drop" "..\\outside";
          remove (jail / ("drop" ^ suffix));
          Alcotest.(check bool) "link entry removed" false (Sys.file_exists (native "jail\\drop"));
          Alcotest.(check (array string)) "outside contents preserved" [|"secret"|] (Sys.readdir (native "outside"));
          Alcotest.(check string) "outside file unchanged" "secret" (read_file (native "outside\\secret"))
        ) [Path.unlink; Path.rmdir; Path.rmtree]
    ) ["/"; "\\"; "//"; "\\\\"; "/."; "\\."; "/\\"]

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
  "stat-directory", `Quick, test_stat_directory env;
  "stat-regular-file", `Quick, test_stat_regular_file env;
  "stat-symlink", `Quick, test_stat_symlink env;
  "rename", `Quick, test_rename env;
  "rename-over-directory", `Quick, test_rename_over_dir env;
  "open-no-follow", `Quick, test_open_no_follow env;
  "eio-symlink", `Quick, test_eio_symlink env;
  "symlink-encoding", `Quick, test_symlink_encoding env;
  "unlink-symlink", `Quick, test_unlink_symlink env;
  "subtree-symlinks", `Quick, test_subtree_symlinks env;
  "no-follow-truncate", `Quick, test_no_follow_truncate env;
  "junction", `Quick, test_junction env;
  "trailing-symlink", `Quick, test_trailing_link ~junction:false env;
  "trailing-junction", `Quick, test_trailing_link ~junction:true env;
  "exclusive-symlink", `Quick, test_exclusive_symlink env;
  "absolute-directory-symlink", `Quick, test_absolute_directory_symlink env;
  "symlink-parent", `Quick, test_symlink_parent env;
  "literal-link-targets", `Quick, test_literal_link_targets env;
  "literal-entries", `Quick, test_literal_entries env;
  "invalid-link-targets", `Quick, test_invalid_link_targets env;
  "literal-symlink-parent", `Quick, test_literal_symlink_parent env;
  "unlink-delete-access", `Quick, test_unlink_delete_access env;
  "cwd-reparse", `Quick, test_cwd_reparse env;
  "unlink-handle", `Quick, test_unlink_handle env;
  "subtree-reparse-read", `Quick, test_subtree_reparse_read env;
  "subtree-reparse-symlink", `Quick, test_subtree_reparse_symlink env;
  "readdir-handle", `Quick, test_readdir_handle env;
]
