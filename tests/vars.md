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
