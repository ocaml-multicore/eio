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
  let cwd = Eio.Stdenv.cwd env in
  try_mkdir (cwd / "sandbox");
  Unix.symlink ~to_dir:true ".." "sandbox\\to-root";
  Unix.symlink ~to_dir:true "subdir" "sandbox\\to-subdir";
  Unix.symlink ~to_dir:true "foo" "sandbox\\dangle";
  try_mkdir (cwd / "tmp");
  Eio.Path.with_open_dir (cwd / "sandbox") @@ fun sandbox ->
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

let tests env = [
  "create-write-read", `Quick, test_create_and_read env;
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
]