(* Walk the directory tree rooted at the current directory,
   showing a summary for any .mli files. *)

let ( / ) = Eio.Path.( / )

let is_doc_comment = String.starts_with ~prefix:"(** "

(* Print the first line of [t]'s doc-comment, if any *)
let scan_mli t f =
  Eio.Path.with_lines t (fun lines ->
      Seq.find is_doc_comment lines
      |> Option.iter (fun line ->
          let stop = String.index_from_opt line 4 '*' |> Option.value ~default:(String.length line) in
          Format.fprintf f "%a: %s@." Eio.Path.pp t (String.sub line 4 (stop - 4))
        )
    )

(* Walk the tree rooted at [t] and scan any .mli files found. *)
let rec scan t f =
  match Eio.Path.kind ~follow:false t with
  | `Directory ->
    Eio.Path.read_dir t |> List.iter (function
        | "_build" | "_opam" -> ()                (* Don't examine these directories *)
        | item when String.starts_with ~prefix:"." item -> () (* Skip hidden items *)
        | item -> scan (t / item) f
      )
  | `Regular_file when Filename.check_suffix (snd t) ".mli" -> scan_mli t f
  | _ -> ()

let () =
  Eio_main.run @@ fun env ->
  scan (Eio.Stdenv.cwd env) Format.std_formatter
