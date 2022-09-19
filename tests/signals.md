# Signal Handling

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std
```

Same signal multiple times
```ocaml
# Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let box1 = Eio.Signal.subscribe ~sw Eio.Signal.sigusr2 in
  Eio.Signal.(publish sigusr2);
  Eio.Signal.(publish sigusr2);
  Eio.Signal.wait box1; traceln "box1";
  Eio.Signal.wait box1; traceln "box1";
  Eio.Signal.unsubscribe box1;
  ;;
+box1
+box1
- : unit = ()
```

One signal and multiple fibers

```ocaml
# Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let box1 = Eio.Signal.subscribe ~sw Eio.Signal.sigusr2 in
  let box2 = Eio.Signal.subscribe ~sw Eio.Signal.sigusr2 in
  let got_box1, got_box2, got_box3 = ref false, ref false, ref false in
  Eio.Fiber.all [
    (fun () -> Eio.Signal.wait box1; got_box1 := true);
    (fun () -> Eio.Signal.wait box2; got_box2 := true);
    (fun () -> Eio.Signal.wait_one Eio.Signal.sigusr2; got_box3 := true);
    (fun () -> Eio.Signal.(publish sigusr2));
  ];
  Eio.Signal.unsubscribe box1;
  Eio.Signal.unsubscribe box2;
  assert (!got_box1 && !got_box2 && !got_box3);
  ;;
- : unit = ()
```

Prove we fail on a unsubscribed box

```ocaml
# Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let box1 = Eio.Signal.subscribe ~sw Eio.Signal.sigusr2 in
  Eio.Signal.(publish sigusr2);
  Eio.Signal.wait box1;
  Eio.Signal.unsubscribe box1;
  Eio.Signal.wait box1;;
Exception: Invalid_argument "sigbox is not subscribed".
```

Prove cancelation works

```ocaml
let prove_cancel () =
  let child () =
    Eio_main.run @@ fun _env ->
    Eio.Fiber.first
      (fun () -> Eio.Signal.(wait_one sigusr2))
      (fun () -> ());
    Eio.Signal.(publish sigusr2) (* suicide *)
  in
  let parent pid =
    match Unix.(waitpid [] pid) with
    | pid', Unix.WSIGNALED signum ->
      assert (pid = pid');
      assert (signum = Sys.sigusr2)
    | _ -> failwith "unexpected waitpid return"
  in
  let pid = Unix.fork () in
  if pid = 0 then
    child ()
  else
    parent pid
in
prove_cancel ()
```

Prove we can receive in multiple domains and that handlers are
properly cleared.

```ocaml
let multi ~hang =
  let num_domains = 8 in
  let ptoc = Unix.pipe () in
  let ctop = Unix.pipe () in
  let isigusr1 = Eio.Signal.(signum_to_int sigusr1) in
  let wait_exited pid ec =
    match (Unix.waitpid [] pid) with
    | pid', Unix.WEXITED ec' ->
      assert (pid = pid');
      assert (ec = ec');
    | _ -> failwith "unexpected waitpid return"
  in
  let rec signal_and_wait pid signum =
    Unix.kill pid signum;
    let pid', ec = Unix.(waitpid [ WNOHANG ] pid) in
    if pid' <> 0 then (
      assert (pid = pid');
      match ec with
      | Unix.WSIGNALED signum' -> assert (signum = signum')
      | _ -> failwith "XXX unexpected waitpid return"
    ) else (
      Unix.sleepf 0.02;
        signal_and_wait pid signum (* XXX is this tailrec !? *)
    )
  in
  let write_byte fd byte =
    let b = Bytes.create 1 in
    Bytes.set_uint8 b 0 byte;
    let n = Unix.single_write fd b 0 1 in
    assert (n = 1)
  in
  let read_byte fd =
    let b = Bytes.create 1 in
    let n = Unix.read fd b 0 1 in
    assert (n = 1);
    Bytes.get_uint8 b 0
  in
  let parent pid =
    Unix.close (snd ctop);
    Unix.close (fst ptoc);
    let input = fst ctop in
    let _output = snd ptoc in
    let wait_domains v =
      let rec loop n =
        if n = 0 then
          ()
        else
          let () = assert ((read_byte input) = v) in
          loop (pred n)
      in
      loop num_domains
    in
    wait_domains 255;
    Unix.kill pid isigusr1;
    wait_domains isigusr1;
    (* if we told our  child to hang, at this point all handlers are being
     * removed, we want to send another sigusr1 to terminate it, thus
     * proving we restored the default signal behaviour *)
    let () =
      if hang then
        signal_and_wait pid Sys.sigusr1 (* we're the parent *)
      else
        wait_exited pid 0
    in
    Unix.close (fst ctop);
	Unix.close (snd ptoc)
  in
  let child () =
    Unix.close (snd ptoc);
    Unix.close (fst ctop);
    let _input = fst ptoc in
    let output = snd ctop in

    Eio_main.run
      (fun env ->
         let domain_manager = Eio.Stdenv.domain_mgr env in
         let clock = Eio.Stdenv.clock env in
         let domain_main _i () =
           Eio.Domain_manager.run domain_manager (fun () ->
               Eio.Fiber.all [
                 (fun () ->
                    Eio.Signal.wait_one Eio.Signal.sigusr1;
                    write_byte output isigusr1);

                 (fun () ->
                    (* let parent know we're up *)
                    write_byte output 255);

                 (fun () ->
                    while hang do
                     Eio.Time.sleep clock 60.0
                   done)
               ])
         in
         Eio.Fiber.all (List.init num_domains domain_main));
    Unix._exit 0
  in
  let pid = Unix.fork () in
  if pid <> 0 then
    parent pid
  else
    child ()
```

```ocaml
# multi ~hang:false;;
- : unit = ()
# multi ~hang:true;;
- : unit = ()
```
