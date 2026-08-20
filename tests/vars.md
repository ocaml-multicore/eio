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
```

# Test cases

Putting and getting environment variables works:

```ocaml
# run @@ fun ~vars ->
  let name = "EIO_TEST_VARIABLE" in
  Eio.Vars.put vars ~name ~value:"hello";
  Eio.Vars.get vars name;;
- : string = "hello"
```

Getting environment variables that do not exist works:

```ocaml
# run @@ fun ~vars ->
  Eio.Vars.get vars "THIS_ENV_PROBABLY_WILL_NOT_EXIST";;
Exception: Not_found.
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
