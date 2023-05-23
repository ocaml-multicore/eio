## Setting up the environment

```ocaml
# #require "eio_main";;
# #require "eio.mock";;
```

```ocaml
open Eio.Std

module T = Eio.Task_pool
```

TODO: replace this copy with one from eio.mock when that's landed

```ocaml
let with_domain_tracing id fn =
  let traceln ?__POS__ fmt =
    Eio.Private.Debug.default_traceln ?__POS__ ("[%d] " ^^ fmt) id
  in
  Fiber.with_binding Eio.Private.Debug.v#traceln { traceln } fn

let fake_domain_mgr () = object (_ : #Eio.Domain_manager.t)
  val mutable next_domain_id = 1

  method run fn =
    let self = next_domain_id in
    next_domain_id <- next_domain_id + 1;
    let cancelled, _ = Promise.create () in
    with_domain_tracing self (fun () -> fn ~cancelled)

  method run_raw _ = assert false
end

let run ~use_fake_domain_mgr (fn : Switch.t -> Eio.Domain_manager.t -> unit) =
  Eio_main.run @@ fun env ->
  let domain_mgr =
    if use_fake_domain_mgr then
      fake_domain_mgr ()
    else
      Eio.Stdenv.domain_mgr env
  in
  Switch.run @@ fun sw ->
  fn sw domain_mgr
```

# Test cases

Three concurrent traceln runners at different frequencies using deterministic
domain manager

```ocaml
# run ~use_fake_domain_mgr:true (fun sw domain_mgr ->
    let pool = T.create ~sw ~max_domains:3 domain_mgr in
    T.async pool (fun () ->
      for _=1 to 6 do
        traceln "0";
        Unix.sleepf 0.2;
      done
    );
    T.async pool (fun () ->
      for _=1 to 3 do
        traceln "1";
        Unix.sleepf 0.4;
      done
    );
    T.async pool (fun () ->
      for _=1 to 2 do
        traceln "2";
        Unix.sleepf 0.6;
      done
    );
    T.clear pool
  );;
+[1] 0
+[1] 0
+[1] 0
+[1] 0
+[1] 0
+[1] 0
+[2] 1
+[2] 1
+[2] 1
+[3] 2
+[3] 2
- : unit = ()
```

Three concurrent traceln runners at different frequencies using native
domain manager

```ocaml
# run ~use_fake_domain_mgr:false (fun sw domain_mgr ->
    let pool = T.create ~sw ~max_domains:3 domain_mgr in
    T.async pool (fun () ->
      for _=1 to 6 do
        traceln "0";
        Unix.sleepf 0.2;
      done
    );
    T.async pool (fun () ->
      for _=1 to 3 do
        traceln "1";
        Unix.sleepf 0.4;
      done
    );
    T.async pool (fun () ->
      for _=1 to 2 do
        traceln "2";
        Unix.sleepf 0.6;
      done
    );
    T.clear pool
  );;
+0
+2
+1
+0
+0
+1
+0
+2
+0
+1
+0
- : unit = ()
```
