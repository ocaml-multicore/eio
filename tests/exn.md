# Setting up the environment

```ocaml
# #require "eio.mock";;
```

Adjust this to test backtrace printing:
```ocaml
let () = Printexc.record_backtrace false
```

```ocaml
let non_io a =
  try failwith a
  with ex -> ex, Printexc.get_raw_backtrace ()

let not_found =
  try raise @@ Eio.Fs.err (Not_found Eio_mock.Simulated_failure)
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    let ex = Eio.Exn.add_context ex "opening file 'foo'" in
    ex, bt

let denied =
  try raise @@ Eio.Fs.err (Permission_denied Eio_mock.Simulated_failure)
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    let ex = Eio.Exn.add_context ex "saving file 'bar'" in
    ex, bt

let combine a b =
  fst @@ Eio.Exn.combine a b
```

## Combining exceptions

Combining regular exceptions:

```ocaml
# raise @@ combine (non_io "a") (non_io "b");;
Exception: Multiple exceptions:
- Failure("a")
- Failure("b")
```

An IO error and a regular exception becomes a regular (non-IO) multiple exception:

```ocaml
# raise @@ combine (non_io "a") not_found;;
Exception:
Multiple exceptions:
- Failure("a")
- Eio.Io Fs Not_found Simulated_failure,
    opening file 'foo'
```

Combining IO exceptions produces another IO exception,
so that if you want to e.g. log all IO errors and continue then that still works:

```ocaml
# Fmt.pr "%a@." Eio.Exn.pp (combine denied not_found);;
Eio.Io Multiple_io
- Fs Permission_denied Simulated_failure, saving file 'bar'
- Fs Not_found Simulated_failure, opening file 'foo'
- : unit = ()
```

They form a tree, because the context information may be useful too:

```ocaml
let combined =
  let e = Eio.Exn.combine denied not_found in
  let ex = Eio.Exn.add_context (fst e) "processing request" in
  ex, snd e
```

```ocaml
# Fmt.pr "%a@." Eio.Exn.pp (combine combined not_found);;
Eio.Io Multiple_io
- Multiple_io
  - Fs Permission_denied Simulated_failure, saving file 'bar'
  - Fs Not_found Simulated_failure, opening file 'foo', processing request
- Fs Not_found Simulated_failure, opening file 'foo'
- : unit = ()
```
