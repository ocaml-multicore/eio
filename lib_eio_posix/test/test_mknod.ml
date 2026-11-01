(* Tests for Low_level.mknod and Eio_unix.Dev. Everything here must pass unprivileged. *)

open Eio.Std

module L = Eio_posix.Low_level
module Dev = Eio_unix.Dev

let dev = Alcotest.testable Dev.pp Eio.File.Dev.equal
let kind = Alcotest.testable Eio.File.Stat.pp_kind (=)

let kind_of dirfd path =
  let buf = L.create_stat () in
  L.fstatat ~buf ~follow:false dirfd path;
  L.kind buf

let expect_unix_error code fn =
  match fn () with
  | () -> Alcotest.failf "Expected %s, but call succeeded" (Unix.error_message code)
  | exception Unix.Unix_error (c, _, _) when c = code -> ()

let expect_rejected fn =
  match fn () with
  | () -> Alcotest.fail "Sandbox escape should have been rejected!"
  | exception Eio.Io (Eio.Fs.E Permission_denied _, _) -> ()

let with_test_dir fn () =
  Eio_posix.run @@ fun env ->
  Eio.Path.(rmtree ~missing_ok:true (Eio.Stdenv.cwd env / "mknod_test"));
  Unix.mkdir "mknod_test" 0o700;
  fn env

let test_dev_roundtrip () =
  List.iter (fun major ->
      List.iter (fun minor ->
          let d = Dev.make ~major ~minor in
          Alcotest.(check int) "major" major (Dev.major d);
          Alcotest.(check int) "minor" minor (Dev.minor d);
          let d' = Dev.make ~major:(Dev.major d) ~minor:(Dev.minor d) in
          Alcotest.check dev "round-trip" d d'
        ) [0; 1; 255; 256; 0xfffff]
    ) [0; 1; 255]

let test_dev_range () =
  let rejected ~major ~minor =
    match Dev.make ~major ~minor with
    | d -> Alcotest.failf "Dev.make ~major:%d ~minor:%d should have been \
                           rejected, but gave %a" major minor Dev.pp d
    | exception Invalid_argument _ -> ()
  in
  rejected ~major:(-1) ~minor:0;
  rejected ~major:0 ~minor:(-1);
  rejected ~major:max_int ~minor:0;
  rejected ~major:0 ~minor:max_int;
  Alcotest.(check string) "pp" "1:3" (Fmt.str "%a" Dev.pp (Dev.make ~major:1 ~minor:3))

let test_dev_of_real_node () =
  Eio_posix.run @@ fun env ->
  let st = Eio.Path.stat ~follow:true Eio.Path.(Eio.Stdenv.fs env / "/dev/null") in
  Alcotest.check kind "kind" `Character_special st.kind;
  match st.rdev with
  | None -> Alcotest.fail "/dev/null should have an rdev!"
  | Some d ->
    let d' = Dev.make ~major:(Dev.major d) ~minor:(Dev.minor d) in
    Alcotest.check dev "round-trip" d d';
    traceln "/dev/null is device %a" Dev.pp d

let test_fifo env =
  L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/fifo";
  Alcotest.check kind "fifo kind" `Fifo (kind_of L.Cwd "mknod_test/fifo");
  let st = Eio.Path.stat ~follow:false Eio.Path.(Eio.Stdenv.cwd env / "mknod_test" / "fifo") in
  Alcotest.check kind "stat kind" `Fifo st.kind;
  Alcotest.(check (option dev)) "no rdev" None st.rdev

let test_eexist _env =
  L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/fifo";
  expect_unix_error Unix.EEXIST (fun () ->
      L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/fifo")

let test_enoent _env =
  expect_unix_error Unix.ENOENT (fun () ->
      L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/missing/fifo")

let test_escape_dotdot _env =
  expect_rejected (fun () ->
      L.mknod `Fifo ~perm:0o600 L.Cwd "../escape-fifo")

let test_escape_symlink _env =
  Unix.symlink "../escape-target" "mknod_test/up";
  Switch.run @@ fun sw ->
  let test_dir = L.openat ~sw ~mode:0 L.Cwd "mknod_test" L.Open_flags.directory in
  expect_rejected (fun () -> L.mknod `Fifo ~perm:0o600 (L.Fd test_dir) "up/escape-fifo")

let () =
  let open Alcotest in
  run "mknod" [
    "dev", [
      test_case "roundtrip" `Quick test_dev_roundtrip;
      test_case "range"     `Quick test_dev_range;
      test_case "real-node" `Quick test_dev_of_real_node;
    ];
    "mknod", [
      test_case "fifo"           `Quick (with_test_dir test_fifo);
      test_case "eexist"         `Quick (with_test_dir test_eexist);
      test_case "enoent"         `Quick (with_test_dir test_enoent);
      test_case "escape-dotdot"  `Quick (with_test_dir test_escape_dotdot);
      test_case "escape-symlink" `Quick (with_test_dir test_escape_symlink);
    ];
  ]
