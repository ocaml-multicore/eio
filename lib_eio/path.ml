type 'a t = 'a Fs.dir * Fs.path

let ( / ) (dir, p1) p2 =
  match p1, p2 with
  | p1, "" -> (dir, Filename.concat p1 p2)
  | _, p2 when not (Filename.is_relative p2) -> (dir, p2)
  | ".", p2 -> (dir, p2)
  | p1, p2 -> (dir, Filename.concat p1 p2)

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
  try X.open_dir dir ~sw path, ""
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

let kind  ~follow t = stat ~follow t [File.Kind] Fun.id
let size  ~follow t = stat ~follow t [File.Size] Fun.id
let perm  ~follow t = stat ~follow t [File.Perm] Fun.id
let uid   ~follow t = stat ~follow t [File.Uid] Fun.id
let gid   ~follow t = stat ~follow t [File.Gid] Fun.id
let atime ~follow t = stat ~follow t [File.Atime] Fun.id
let mtime ~follow t = stat ~follow t [File.Mtime] Fun.id
let ctime ~follow t = stat ~follow t [File.Ctime] Fun.id

let exists t =
  try
    stat ~follow:true t [File.Kind]
    (function `Directory | `Regular_file -> true | _ -> false)
  with Exn.Io (Fs.E Not_found _, _) -> false

let is_file t =
  try
    stat ~follow:true t [File.Kind] ((=) `Regular_file)
  with Exn.Io (Fs.E Not_found _, _) -> false

let is_directory t =
  try
    stat ~follow:true t [File.Kind] ((=) `Directory)
  with Exn.Io (Fs.E Not_found _, _) -> false

let with_open_in path fn =
  Switch.run @@ fun sw -> fn (open_in ~sw path)

let with_open_out ?append ~create path fn =
  Switch.run @@ fun sw -> fn (open_out ~sw ?append ~create path)

let with_open_dir path fn =
  Switch.run @@ fun sw -> fn (open_dir ~sw path)

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

let rename t1 t2 =
  let (dir2, new_path) = t2 in
  let (Resource.T (dir, ops), old_path) = t1 in
  let module X = (val (Resource.get ops Fs.Pi.Dir)) in
  try X.rename dir old_path (dir2 :> _ Fs.dir) new_path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "renaming %a to %a" pp t1 pp t2
