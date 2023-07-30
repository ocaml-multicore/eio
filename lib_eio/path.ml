type 'a t = (#Fs.dir as 'a) * Fs.path

let ( / ) (dir, p1) p2 =
  match p1, p2 with
  | p1, "" -> (dir, Filename.concat p1 p2)
  | _, p2 when not (Filename.is_relative p2) -> (dir, p2)
  | ".", p2 -> (dir, p2)
  | p1, p2 -> (dir, Filename.concat p1 p2)

let pp f ((t:#Fs.dir), p) =
  if p = "" then Fmt.pf f "<%t>" t#pp
  else Fmt.pf f "<%t:%s>" t#pp (String.escaped p)

let split_last_seg p =
  (* Get the last instance of a character in a string *)
  let rec rindex i c path =
    if i < 0 then None else
    if Char.equal (String.unsafe_get path i) c
    then Some i
    else rindex (i - 1) c path
  in
  (* Trim the last instances of a character in a string *)
  let rtrim c path =
    let len = String.length path in
    let rec remove i =
      if i > len then i else
      if Char.equal (String.unsafe_get path (len - 1 - i)) c then remove (i + 1)
      else i
    in
    let remove_last = remove 0 in
    String.sub path 0 (len - remove_last)
  in
  let len = String.length p - 1 in
  match rindex len '/' p with
  | None -> None, Some p
  | Some idx ->
    (* We want the separator in the head not the tail *)
    let idx = idx + 1 in
    let child = String.sub p idx (len - idx + 1) in
    Some (rtrim '/' @@ String.sub p 0 idx), (if String.equal child "" then None else Some child)

let open_in ~sw ((t:#Fs.dir), path) =
  try t#open_in ~sw path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "opening %a" pp (t, path)

let open_out ~sw ?(append=false) ~create ((t:#Fs.dir), path) =
  try t#open_out ~sw ~append ~create path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "opening %a" pp (t, path)

let open_dir ~sw ((t:#Fs.dir), path) =
  try (t#open_dir ~sw path, "")
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "opening directory %a" pp (t, path)

let mkdir ~perm ((t:#Fs.dir), path) =
  try t#mkdir ~perm path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "creating directory %a" pp (t, path)

let read_dir ((t:#Fs.dir), path) =
  try List.sort String.compare (t#read_dir path)
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "reading directory %a" pp (t, path)

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

let unlink ((t:#Fs.dir), path) =
  try t#unlink path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "removing file %a" pp (t, path)

let rmdir ((t:#Fs.dir), path) =
  try t#rmdir path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "removing directory %a" pp (t, path)

let rename ((t1:#Fs.dir), old_path) (t2, new_path) =
  try t1#rename old_path (t2 :> Fs.dir) new_path
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "renaming %a to %a" pp (t1, old_path) pp (t2, new_path)

(* TODO: An exists function that doesn't need to open the file
   but that is still capability-like ? *)
let exists path =
  try
    with_open_in path @@ fun _ -> true
  with
  | Exn.Io ((Fs.E Fs.Not_found _), _) -> false

let mkdirs ?(exists_ok=false) ~perm ((t:#Fs.dir), path) =
  let rec loop name =
    let parent, child =
      match split_last_seg name with
      | (Some _ as p), (Some _ as c) -> p, c
      | Some parent, None -> split_last_seg parent
      | None, _ -> None, None
    in
    let () =
      match parent, child with
      | Some parent, Some _ -> (
        try
          if not (exists (t, parent)) then loop parent
        with
        | Exn.Io ((Fs.E Fs.Permission_denied _), _) as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Exn.reraise_with_context exn bt "whilst creating directory %a" pp (t, path)
      )
      | _ -> ()
    in
    try t#mkdir ~perm name with
    | Exn.Io ((Fs.E Fs.Already_exists _), _) as exn ->
      if not exists_ok then
        let bt = Printexc.get_raw_backtrace () in
        Exn.reraise_with_context exn bt "creating directory %a" pp (t, path)
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      Exn.reraise_with_context exn bt "creating directory %a" pp (t, path)
  in
  loop path
