type t = Eio.Mutex.t

let split_in_two c s =
  try
    let first_occurrence = String.index s c in
    let first = String.sub s 0 first_occurrence in
    let second = String.sub s (first_occurrence + 1) (String.length s - first_occurrence - 1) in
    Some (first, second)
  with Not_found -> None

let get_all m =
  Eio.Mutex.use_ro m @@ fun () ->
  Unix.environment () |> Array.to_list |> List.filter_map (split_in_two '=')

let get m name =
  Eio.Mutex.use_ro m @@ fun () ->
  Unix.getenv name

let put m ~name ~value =
  Eio.Mutex.use_rw ~protect:false m @@ fun () ->
  Unix.putenv name value

let get_path ~sep t =
  match get t "PATH" with
  | "" -> []
  | paths -> String.split_on_char sep paths

let put_path ~sep t paths =
  let paths = String.concat sep paths in
  put t ~name:"PATH" ~value:paths
