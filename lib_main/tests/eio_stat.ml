(* stat(1) using eio *)

let pp_time f t =
  let tm = Unix.localtime t in
  Format.fprintf f "%04d-%02d-%02d %02d:%02d:%02d +0000"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let pp_perm_hum ppf v =
  let rwx i = ((i land 4) = 4), ((i land 2) = 2), (i land 1 = 1) in
  let pp_rwx ppf (r, w, x) =
    let is_set c v = if v then c else "-" in
    Format.fprintf ppf "%s%s%s" (is_set "r" r) (is_set "w" w) (is_set "x" x);
  in
  Format.fprintf ppf "%a%a%a" pp_rwx (rwx (v lsr 6)) pp_rwx (rwx (v lsr 3)) pp_rwx (rwx v)

let run_stat fs fname =
  let open Eio in
  let path = Path.(fs / fname) in
  let opt_symlink = function
    | `Symbolic_link -> " -> "^(Unix.readlink fname) (* TODO need an eio readlink *)
    | _ -> ""  in
  Path.stat ~follow:false path File.[Kind; Size; Dev; Ino; Nlink; Perm; Uid; Gid; Atime; Mtime; Ctime]
    (fun kind size dev ino nlink perm uid gid atime mtime ctime ->
      Format.printf "  File: %s%s\n  Size: %Lu\t\tFileType: %a\nDevice: %Lu\tInode: %Lu\tLinks: %Lu\nMode: (%04o/-%a)\tUid: (%Lu/TODO)\tGid: (%Lu/TODO)\nAccess: %a\nModify: %a\nChange: %a\n%!"
        fname (opt_symlink kind)
        size
        Eio.File.pp_kind kind
        dev
        ino
        nlink
        perm
        pp_perm_hum
        perm
        uid
        gid
        pp_time atime
        pp_time mtime
        pp_time ctime
   )

let () =
  Eio_main.run @@ fun env ->
  (* TODO cmdliner *)
  let fname = Sys.argv.(1) in
  run_stat (Eio.Stdenv.fs env) fname
