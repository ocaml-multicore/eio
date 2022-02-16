```ocaml
# #require "eio.utils";;
```

# A dummy Eio backend with no actual effects

This is very inefficient and not thread-safe, but it demonstrates the idea.
A real backend would typically pass `main` some way to interact with it, like the other backends do.

```ocaml
open Eio.Std

(* An Eio backend with no actual IO *)
module Eio_null = struct
  module Fibre_context = Eio.Private.Fibre_context

  (* The scheduler could just return [unit], but this is clearer. *)
  type exit = Exit_scheduler

  type t = {
    (* Suspended fibres waiting to run again.
       A real system would probably use [Eio_utils.Lf_queue]. *)
    mutable run_q : (unit -> exit) list;
  }

  (* Resume the next runnable fibre, if any. *)
  let schedule t : exit =
    match t.run_q with
    | f :: fs -> t.run_q <- fs; f ()
    | [] -> Exit_scheduler      (* Finished (or deadlocked) *)

  (* Run [main] in an Eio main loop. *)
  let run main =
    let t = { run_q = [] } in
    let rec fork ~new_fibre:fibre fn =
      let open Eio.Private.Effect in
      (* Create a new fibre and run [fn] in it. *)
      Deep.match_with fn ()
        { retc = (fun () -> Fibre_context.destroy fibre; schedule t);
          exnc = (fun ex -> Fibre_context.destroy fibre; raise ex);
          effc = fun (type a) (e : a eff) : ((a, exit) Deep.continuation -> exit) option ->
            match e with
            | Eio.Private.Effects.Suspend f -> Some (fun k ->
                (* Ask [f] to register whatever callbacks are needed to resume the fibre.
                   e.g. it might register a callback with a promise, for when that's resolved. *)
                f fibre (function
                    (* The fibre is ready to run again. Add it to the queue. *)
                    | Ok v -> t.run_q <- t.run_q @ [fun () -> Deep.continue k v]
                    | Error ex -> t.run_q <- t.run_q @ [fun () -> Deep.discontinue k ex]
                  );
                (* Switch to the next runnable fibre while this one's blocked. *)
                schedule t
              )
            | Eio.Private.Effects.Fork (new_fibre, f) -> Some (fun k ->
                (* Arrange for the forking fibre to run immediately after the new one. *)
                t.run_q <- Deep.continue k :: t.run_q;
                (* Create and run the new fibre (using fibre context [new_fibre]). *)
                fork ~new_fibre (fun () ->
                    try f ()
                    with _ ->
                      (* Fibre.fork handles exceptions for us.
                         This is just to allow for tracing. *)
                      ()
                  )
              )
            | Eio.Private.Effects.Get_context -> Some (fun k ->
                Deep.continue k fibre
              )
            | Eio.Private.Effects.Trace -> Some (fun k ->
                Deep.continue k Eio_utils.Trace.default_traceln
              )
            | _ -> None
        }
    in
    let new_fibre = Fibre_context.make_root () in
    let Exit_scheduler = fork ~new_fibre main in
    ()
end
```

It supports forking, tracing, suspending and cancellation:

```ocaml
# Eio_null.run @@ fun () ->
  let s = Eio.Stream.create 1 in
  try
    Fibre.both
      (fun () ->
         for x = 1 to 3 do
           traceln "Sending %d" x;
           Eio.Stream.add s x
         done;
         raise Exit
      )
      (fun () ->
         while true do
           traceln "Got %d" (Eio.Stream.take s)
         done
      )
  with Exit ->
    traceln "Finished!";;
+Sending 1
+Sending 2
+Got 1
+Got 2
+Sending 3
+Got 3
+Finished!
- : unit = ()
```
