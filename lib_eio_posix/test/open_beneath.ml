open Eio.Std

let () = Printexc.record_backtrace true

module L = Eio_posix.Low_level

let check ~mode dirfd path flags =
  Switch.run @@ fun sw ->
  (* traceln "check %S" path; *)
  let x =
    let path = if path = "" then "." else path in
    try Ok (L.Resolve.open_unconfined ~sw ~mode (Some dirfd) path flags) with Unix.Unix_error _ as e -> Error e in
  let y =
    Eio_unix.Fd.use_exn "check" dirfd @@ fun dirfd ->
    try Ok (L.Resolve.open_beneath_fallback ~sw ~dirfd ~mode path flags) with
    | Unix.Unix_error _ as e -> Error e
    | Eio.Io _ as e -> Error e
  in
  match x, y with
  | Ok x, Ok y ->
    let inode fd =
      let buf = L.create_stat () in
      L.fstat fd ~buf;
      (L.dev buf, L.ino buf)
    in
    let x_info = inode x in
    let y_info = inode y in
    if x_info <> y_info then
      Fmt.failwith "Got a different inode opening %S!" path
  | Error (Unix.Unix_error (x, _, _) as e1),
    Error (Unix.Unix_error (y, _, _) as e2) ->
    if x <> y then (
      Fmt.failwith "Different errors: %a vs %a" Fmt.exn e1 Fmt.exn e2
    )
  | Error (Unix.Unix_error _), Error (Eio.Io (Eio.Fs.E Permission_denied _, _)) -> ()
  | Error e1, Error e2 -> Fmt.failwith "Multiple errors: %a vs %a" Fmt.exn e1 Fmt.exn e2
  | Error e, Ok _ -> Fmt.failwith "Only OS open failed: %a" Fmt.exn e
  | Ok _, Error e -> Fmt.failwith "Only open_beneath failed: %a" Fmt.exn e

let test base path =
  check ~mode:0 base path L.Open_flags.rdonly;
  if path <> "" then (
    check ~mode:0 base (path ^ "/") L.Open_flags.rdonly;
    check ~mode:0 base (path ^ "/.") L.Open_flags.rdonly
  )

let test_denied base path =
  match L.Open_flags.resolve_beneath with
  | Some some_resolve_beneath ->
    (* Check our behaviour matches the OS's *)
    check ~mode:0 base path L.Open_flags.(rdonly + some_resolve_beneath)
  | None ->
    (* traceln "check-reject %S" path; *)
    (* OS doesn't support resolve_beneath. Just check we reject it. *)
    Switch.run @@ fun sw ->
    Eio_unix.Fd.use_exn "check" base @@ fun base ->
    match L.Resolve.open_beneath_fallback ~sw ~dirfd:base ~mode:0 path L.Open_flags.rdonly with
    | (_fd : Eio_unix.Fd.t) -> Fmt.failwith "%S should have been rejected!" path
    | exception Eio.Io (Eio.Fs.E Permission_denied _, _) -> ()

let () =
  try
    Eio_posix.run @@ fun env ->
    Eio.Path.(rmtree ~missing_ok:true (Eio.Stdenv.cwd env / "test_beneath"));
    Unix.mkdir "test_beneath" 0o700;
    Unix.mkdir "test_beneath/subdir" 0o700;
    Unix.symlink "subdir" "test_beneath/link_subdir";
    Unix.symlink ".." "test_beneath/link_subdir/parent";
    Unix.symlink ".." "test_beneath/parent";
    Unix.symlink "loop2" "test_beneath/loop1";
    Unix.symlink "loop1" "test_beneath/loop2";
    Unix.symlink "file" "test_beneath/to-file";
    Unix.symlink "file/" "test_beneath/to-file-slash";
    Unix.symlink "subdir/" "test_beneath/to-dir-slash";
    Switch.run @@ fun sw ->
    let test_dir = L.Resolve.open_beneath_fallback ~sw "test_beneath" L.Open_flags.directory ~mode:0 in
    let f = L.openat ~sw (Fd test_dir) "file" L.Open_flags.(creat + rdwr) ~mode:0o600 in
    Eio_unix.Fd.close f;
    test test_dir "file";
    test test_dir "subdir";
    test test_dir "link_subdir";
    test test_dir "link_subdir/parent";
    test_denied test_dir "link_subdir/parent/parent";
    test test_dir "";
    test test_dir ".";
    test_denied test_dir "..";
    test test_dir "loop1";
    test test_dir "to-file";
    test test_dir "to-file-slash";
    test test_dir "to-dir-slash/file";
  with Failure msg ->
    Printexc.print_backtrace stderr;
    Fmt.epr "Tests failed: %s" msg;
    exit 1
