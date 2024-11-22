type 'a t = 'a Fs.dir * Fs.path

(* Like [Filename.is_relative] but always using "/" as the separator. *)
let is_relative = function
  | "" -> true
  | x -> x.[0] <> '/'

(* Like [Filename.concat] but always using "/" as the separator. *)
let concat a b =
  let l = String.length a in
  if l = 0 || a.[l - 1] = '/' then a ^ b
  else a ^ "/" ^ b

let ( / ) (dir, p1) p2 =
  match p1, p2 with
  | p1, "" -> (dir, concat p1 p2)
  | _, p2 when not (is_relative p2) -> (dir, p2)
  | ".", p2 -> (dir, p2)
  | p1, p2 -> (dir, concat p1 p2)

let pp f (Resource.T (t, ops), p) =
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  if p = "" then Fmt.pf f "<%a>" X.pp t
  else Fmt.pf f "<%a:%s>" X.pp t (String.escaped p)

let native (Resource.T (t, ops), p) =
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  X.native t p

let native_exn t =
  match native t with
  | Some p -> p
  | None -> raise (Fs.err (Not_native (Fmt.str "%a" pp t)))

(* Drop the first [n] characters from [s]. *)
let string_drop s n =
  String.sub s n (String.length s - n)

(* "/foo/bar//" -> "/foo/bar"
   "///" -> "/"
   "foo/bar" -> "foo/bar"
 *)
let remove_trailing_slashes s =
  let rec aux i =
    if i <= 1 || s.[i - 1] <> '/' then (
      if i = String.length s then s
      else String.sub s 0 i
    ) else aux (i - 1)
  in
  aux (String.length s)

let split (dir, p) =
  match remove_trailing_slashes p with
  | "" -> None
  | "/" -> None
  | p ->
    match String.rindex_opt p '/' with
    | None -> Some ((dir, ""), p)
    | Some idx ->
      let basename = string_drop p (idx + 1) in
      let dirname =
        if idx = 0 then "/"
        else remove_trailing_slashes (String.sub p 0 idx)
      in
      Some ((dir, dirname), basename)

let open_in ~sw t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.open_in dir ~sw path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "opening %a" pp t

let open_out ~sw ?(append=false) ~create t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.open_out dir ~sw ~append ~create path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "opening %a" pp t

let open_dir ~sw t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try
    let sub = X.open_dir dir ~sw path, "" in
    (sub : [`Close | `Dir] t :> [< `Close | `Dir] t)
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "opening directory %a" pp t

let mkdir ~perm t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.mkdir dir ~perm path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "creating directory %a" pp t

let read_dir t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try List.sort String.compare (X.read_dir dir path)
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "reading directory %a" pp t

let stat ~follow t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.stat ~follow dir path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "examining %a" pp t

let kind ~follow t =
  try ((stat ~follow t).kind :> [File.Stat.kind | `Not_found])
  with Exn.Io (Fs.E Not_found _, _) -> `Not_found

let is_file t =
  kind ~follow:true t = `Regular_file

let is_directory t =
  kind ~follow:true t = `Directory

let with_open_in path fn =
  Switch.run ~name:"with_open_in" @@ fun sw -> fn (open_in ~sw path)

let with_open_out ?append ~create path fn =
  Switch.run ~name:"with_open_out" @@ fun sw -> fn (open_out ~sw ?append ~create path)

let with_open_dir path fn =
  Switch.run ~name:"with_open_dir" @@ fun sw -> fn (open_dir ~sw path)

let with_lines path fn =
  with_open_in path @@ fun flow ->
  let buf = Buf_read.of_flow flow ~max_size:max_int in
  fn (Buf_read.lines buf)

let load (t, path) =
  with_open_in (t, path) @@ fun flow ->
  try
    let size = File.size flow in
    if Optint.Int63.(compare size (of_int Sys.max_string_length)) = 1 then
      raise @@ Fs.err File_too_large;
    let buf = Cstruct.create (Optint.Int63.to_int size) in
    let rec loop buf got =
      match Flow.single_read flow buf with
      | n -> loop (Cstruct.shift buf n) (n + got)
      | exception End_of_file -> got
    in
    let got = loop buf 0 in
    Cstruct.to_string ~len:got buf
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "loading %a" pp (t, path)

let save ?append ~create path data =
  with_open_out ?append ~create path @@ fun flow ->
  Flow.copy_string data flow

let unlink t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.unlink dir path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "removing file %a" pp t

let rmdir t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.rmdir dir path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "removing directory %a" pp t

let catch_missing ~missing_ok fn x =
  if missing_ok then
    try fn x
    with Exn.Io (Fs.E Not_found _, _) -> ()
  else fn x

let rec rmtree ~missing_ok t =
  match kind ~follow:false t with
  | `Directory ->
    Switch.run ~name:"rmtree" (fun sw ->
        match
          let t = open_dir ~sw t in
          t, read_dir t
        with
        | t, items -> List.iter (fun x -> rmtree ~missing_ok (t / x)) items
        | exception Exn.Io (Fs.E Not_found _, _) when missing_ok -> ()
    );
    catch_missing ~missing_ok rmdir t
  | `Not_found when missing_ok -> ()
  | _ ->
    catch_missing ~missing_ok unlink t

let rmtree ?(missing_ok=false) t =
  rmtree ~missing_ok (t :> Fs.dir_ty t)

let rename t1 t2 =
  let (dir2, new_path) = t2 in
  let (Resource.T (dir, ops), old_path) = t1 in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.rename dir old_path (dir2 :> _ Fs.dir) new_path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "renaming %a to %a" pp t1 pp t2

let symlink ~link_to source =
  let (Resource.T (dir, ops), path) = source in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.symlink dir path ~link_to
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "creating symlink %a -> %s" pp source link_to

let rec mkdirs ?(exists_ok=false) ~perm t =
  (* Check parent exists first. *)
  split t |> Option.iter (fun (parent, _) ->
      match is_directory parent with
      | true -> ()
      | false -> mkdirs ~perm ~exists_ok:true parent
      | exception (Exn.Io _ as ex) ->
        let bt = Printexc.get_raw_backtrace () in
        Exn.reraise_with_context ex bt "creating directory %a" pp t
    );
  try mkdir ~perm t
  with Exn.Io (Fs.E Already_exists _, _) when exists_ok && is_directory t -> ()

let read_link t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.read_link dir path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "reading target of symlink %a" pp t

let chown ~follow ~uid ~gid t =
  let (Resource.T (dir, ops), path) = t in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.chown ~follow ~uid ~gid dir path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "changing ownership of %a" pp t
