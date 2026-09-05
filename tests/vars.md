# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : vars:_ Eio.Vars.t -> 'a) =
  Eio_main.run @@ fun env ->
  let vars = Eio.Stdenv.vars env in
  fn ~vars

let try_get t name =
  try
    Eio.traceln "%s is %a" name Fmt.(quote string) (Eio.Vars.get t name)
  with Not_found ->
    Eio.traceln "%s was not found" name

let try_get_path t =
  try
    Eio.traceln "PATH is %a" Fmt.(brackets (list ~sep:comma (quote string))) (Eio.Vars.get_path t)
  with Not_found ->
    Eio.traceln "PATH was not found"
```

# Test cases

Putting and getting environment variables works:

```ocaml
# run @@ fun ~vars ->
  let name = "EIO_TEST_VARIABLE" in
  Eio.Vars.put vars ~name ~value:"hello";
  try_get vars name;
  Eio.Vars.put vars ~name ~value:"";
  try_get vars name;
  try_get vars "THIS_ENV_PROBABLY_WILL_NOT_EXIST";;
+EIO_TEST_VARIABLE is "hello"
+EIO_TEST_VARIABLE is ""
+THIS_ENV_PROBABLY_WILL_NOT_EXIST was not found
- : unit = ()
```

Getting all the environment variables, but filtering out most for
reproducibility.

```ocaml
# run @@ fun ~vars ->
  let variables = [
  "EIO_TEST_VARIABLE1", "hello";
  "EIO_TEST_VARIABLE2", "=hello";
  "EIO_TEST_VARIABLE3", "hello=";
  ] in
  List.iter (fun (name, value) -> Eio.Vars.put vars ~name ~value) variables;
  Eio.Vars.get_all vars
  |> List.filter (fun (name, _) -> Option.is_some (List.assoc_opt name variables))
  |> List.stable_sort (fun (k1, _) (k2, _) -> String.compare k1 k2);;
- : (string * string) list =
[("EIO_TEST_VARIABLE1", "hello"); ("EIO_TEST_VARIABLE2", "=hello");
 ("EIO_TEST_VARIABLE3", "hello=")]
```

Getting and setting the `PATH` variable:

```ocaml
# run @@ fun ~vars ->
  Eio.Vars.put_path vars [ "/usr/bin"; "/bin"; "/usr/local/bin"; ];
  try_get_path vars;
  Eio.Vars.put_path vars [];
  try_get_path vars;
  Eio.Vars.put_path vars ["."; "/foo"];
  try_get_path vars;
  Eio.Vars.put_path vars [ "/foo"; ""; "/bar" ];
  try_get_path vars;
  Eio.Vars.put_path vars [ "/foo  /bar" ];
  try_get_path vars
+PATH is ["/usr/bin", "/bin", "/usr/local/bin"]
+PATH is []
+PATH is [".", "/foo"]
+PATH is ["/foo", "", "/bar"]
+PATH is ["/foo  /bar"]
- : unit = ()
```

