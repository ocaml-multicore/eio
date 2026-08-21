type t = unit

let split_in_two c s =
  try
    let first_occurence = String.index s c in
    let first = String.sub s 0 first_occurence in
    let second = String.sub s (first_occurence + 1) (String.length s - first_occurence - 1) in
    Some (first, second)
  with Not_found -> None

let get_all () =
  Unix.environment () |> Array.to_list |> List.filter_map (split_in_two '=')

let get () name =
  Unix.getenv name

let put () ~name ~value =
  Unix.putenv name value

let get_path ~sep t =
  match get t "PATH" with
  | "" -> []
  | paths -> String.split_on_char sep paths

let put_path ~sep t paths =
  let paths = String.concat sep paths in
  put t ~name:"PATH" ~value:paths
