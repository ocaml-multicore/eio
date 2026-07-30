(* Tests for Low_level.mknod. Everything here must pass unprivileged. *)

open Eio.Std

module L = Eio_posix.Low_level

let kind_of dirfd path =
  let buf = L.create_stat () in
  L.fstatat ~buf ~follow:false dirfd path;
  L.kind buf

let expect_unix_error code fn =
  match fn () with
  | () -> Fmt.failwith "Expected %s, but call succeeded" (Unix.error_message code)
  | exception Unix.Unix_error (c, _, _) when c = code -> ()

let expect_rejected fn =
  match fn () with
  | () -> failwith "Sandbox escape should have been rejected!"
  | exception Eio.Io (Eio.Fs.E Permission_denied _, _) -> ()

(* Check major/minor survive a round-trip through [Dev.t]. *)
let test_dev_roundtrip () =
  List.iter (fun major ->
      List.iter (fun minor ->
          let d = L.Dev.make ~major ~minor in
          if L.Dev.major d <> major || L.Dev.minor d <> minor then
            Fmt.failwith "Dev round-trip failed: %d:%d became %d:%d"
              major minor (L.Dev.major d) (L.Dev.minor d);
          let d' = L.Dev.make ~major:(L.Dev.major d) ~minor:(L.Dev.minor d) in
          assert (L.Dev.to_int64 d' = L.Dev.to_int64 d)
        ) [0; 1; 255; 256; 0xfffff]
    ) [0; 1; 255]

(* [Dev.make] range-checks by round-tripping through the C macros. *)
let test_dev_range () =
  let rejected ~major ~minor =
    match L.Dev.make ~major ~minor with
    | d -> Fmt.failwith "Dev.make ~major:%d ~minor:%d should have been \
                         rejected, but gave %a" major minor L.Dev.pp d
    | exception Invalid_argument _ -> ()
  in
  rejected ~major:(-1) ~minor:0;
  rejected ~major:0 ~minor:(-1);
  rejected ~major:max_int ~minor:0;
  rejected ~major:0 ~minor:max_int;
  assert (Fmt.str "%a" L.Dev.pp (L.Dev.make ~major:1 ~minor:3) = "1:3")

(* A device node's number should decompose/recompos unchanged. *)
let test_dev_of_real_node () =
  let buf = L.create_stat () in
  L.fstatat ~buf ~follow:true L.Fs "/dev/null";
  assert (L.kind buf = `Character_special);
  let d = L.rdev buf in
  let d' = L.Dev.make ~major:(L.Dev.major d) ~minor:(L.Dev.minor d) in
  if L.Dev.to_int64 d' <> L.Dev.to_int64 d then
    Fmt.failwith "/dev/null rdev %a did not round-trip via %d:%d (got %a)"
      L.Dev.pp d (L.Dev.major d) (L.Dev.minor d) L.Dev.pp d';
  traceln "/dev/null is device %a" L.Dev.pp d

let () =
  Eio_posix.run @@ fun env ->
  test_dev_roundtrip ();
  test_dev_range ();
  test_dev_of_real_node ();
  Eio.Path.(rmtree ~missing_ok:true (Eio.Stdenv.cwd env / "mknod_test"));
  Unix.mkdir "mknod_test" 0o700;
  (* Create a FIFO and check its kind. *)
  L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/fifo";
  assert (kind_of L.Cwd "mknod_test/fifo" = `Fifo);
  (* EEXIST on a second attempt. *)
  expect_unix_error Unix.EEXIST (fun () ->
      L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/fifo");
  (* ENOENT for a missing parent directory. *)
  expect_unix_error Unix.ENOENT (fun () ->
      L.mknod `Fifo ~perm:0o600 L.Cwd "mknod_test/missing/fifo");
  (* Escape via ".." from a Cwd-confined dir_fd is rejected. *)
  expect_rejected (fun () ->
      L.mknod `Fifo ~perm:0o600 L.Cwd "../escape-fifo");
  (*  Escape via a symlink pointing above an Fd-confined dir is rejected. *)
  Unix.symlink "../escape-target" "mknod_test/up";
  Switch.run @@ fun sw ->
  let test_dir = L.openat ~sw ~mode:0 L.Cwd "mknod_test" L.Open_flags.directory in
  expect_rejected (fun () -> L.mknod `Fifo ~perm:0o600 (L.Fd test_dir) "up/escape-fifo");
  traceln "test_mknod: ok"
