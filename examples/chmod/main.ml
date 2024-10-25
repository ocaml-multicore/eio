open Eio.Std

let get_permissions path env =
  let fs = Eio.Stdenv.fs env in
  let full_path = Eio.Path.(fs / path) in
  let stats = Eio.Path.stat ~follow:true full_path in
  stats.perm 

let test_chmod env =
  Switch.run @@ fun sw ->
    let fs = Eio.Stdenv.fs env in
    let tmp_dir = Eio.Path.(fs / "tmp") in
    let file_path = Eio.Path.(tmp_dir / "test_file") in
    Eio.Path.(open_out ~sw ~create:(`If_missing 0o600) file_path) |> Eio.Flow.close;

    let permissions = get_permissions (Eio.Path.native_exn file_path) env in
    Printf.printf "Permissions: %o\n" permissions;

    Eio.Path.chmod ~follow:true ~perm:0o600 file_path;

    let new_permissions = get_permissions (Eio.Path.native_exn file_path) env in
    Printf.printf "New Permissions: %o\n" new_permissions;
    ()

let () =
  Eio_main.run @@ fun env ->
    test_chmod env
