type 'a t = (#Fs.dir as 'a) * Fs.path

let ( / ) (dir, p1) p2 =
  match p1, p2 with
  | p1, "" -> (dir, Filename.concat p1 p2)
  | _, p2 when not (Filename.is_relative p2) -> (dir, p2)
  | ".", p2 -> (dir, p2)
  | p1, p2 -> (dir, Filename.concat p1 p2)

let pp f ((t:#Fs.dir), p) =
  Fmt.pf f "<%t:%s>" t#pp (String.escaped p)

let open_in ~sw ((t:#Fs.dir), path) = t#open_in ~sw path
let open_out ~sw ?(append=false) ~create ((t:#Fs.dir), path) = t#open_out ~sw ~append ~create path
let open_dir ~sw ((t:#Fs.dir), path) = (t#open_dir ~sw path, "")
let mkdir ~perm ((t:#Fs.dir), path) = t#mkdir ~perm path
let read_dir ((t:#Fs.dir), path) = List.sort String.compare (t#read_dir path)

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

let load path =
  with_open_in path @@ fun flow ->
  let buf = Buf_read.of_flow flow ~max_size:max_int in
  Buf_read.take_all buf

let save ?append ~create path data =
  with_open_out ?append ~create path @@ fun flow ->
  Flow.copy_string data flow

let unlink ((t:#Fs.dir), path) = t#unlink path
let rmdir ((t:#Fs.dir), path) = t#rmdir path
let rename ((t1:#Fs.dir), old_path) (t2, new_path) = t1#rename old_path (t2 :> Fs.dir) new_path
