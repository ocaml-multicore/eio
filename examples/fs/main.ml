(* Walks the directory tree rooted at the current directory,
   displaying all directory names (skipping hidden directories and `_build`). *)

open Eio.Std

let ( / ) = Eio.Path.( / )

let rec scan t =
  match Eio.Path.kind ~follow:false t with
  | `Directory ->
    traceln "Visiting %a" Eio.Path.pp t;
    Eio.Path.read_dir t |> List.iter (function
      | "_build" -> ()
      | item when String.starts_with ~prefix:"." item -> ()
      | item -> scan (t / item)
    )
  | _ -> ()

let () =
  Eio_main.run @@ fun env ->
  scan (Eio.Stdenv.cwd env)
