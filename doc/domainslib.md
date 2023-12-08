You can resolve an Eio promise from non-Eio domains (or systhreads), which provides an easy way to retrieve the result.
For example:

<!-- $MDX skip -->
```ocaml
open Eio.Std

let pool = Domainslib.Task.setup_pool ~num_domains:2 ()

let fib n = ... (* Some Domainslib function *)

let run_in_pool fn x =
  let result, set_result = Promise.create () in
  let _ : unit Domainslib.Task.promise = Domainslib.Task.async pool (fun () ->
      Promise.resolve set_result @@
      match fn x with
      | r -> Ok r
      | exception ex -> Error ex
    )
  in
  Promise.await_exn result

let () =
  Eio_main.run @@ fun _ ->
  Fiber.both
    (fun () -> traceln "fib 30 = %d" (run_in_pool fib 30))
    (fun () -> traceln "fib 10 = %d" (run_in_pool fib 10))
```
Note that most Domainslib functions can only be called from code running in the Domainslib pool,
while most Eio functions can only be used from Eio domains.
The bridge function `run_in_pool` makes use of the fact that `Domainslib.Task.async` is able to run from
an Eio domain, and `Eio.Promise.resolve` is able to run from a Domainslib one.
