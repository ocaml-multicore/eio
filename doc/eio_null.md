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
  module Fiber_context = Eio.Private.Fiber_context
  module Effect = Eio.Private.Effect    (* For compatibility with 4.12+domains *)

  (* The scheduler could just return [unit], but this is clearer. *)
  type exit = Exit_scheduler

  type t = {
    (* Suspended fibers waiting to run again.
       A real system would probably use [Eio_utils.Lf_queue]. *)
    mutable run_q : (unit -> exit) list;
  }

  (* Resume the next runnable fiber, if any. *)
  let schedule t : exit =
    match t.run_q with
    | f :: fs -> t.run_q <- fs; f ()
    | [] -> Exit_scheduler      (* Finished (or deadlocked) *)

  (* Run [main] in an Eio main loop. *)
  let run main =
    let t = { run_q = [] } in
    let rec fork ~new_fiber:fiber fn =
      (* Create a new fiber and run [fn] in it. *)
      Effect.Deep.match_with fn ()
        { retc = (fun () -> Fiber_context.destroy fiber; schedule t);
          exnc = (fun ex -> Fiber_context.destroy fiber; raise ex);
          effc = fun (type a) (e : a Effect.t) : ((a, exit) Effect.Deep.continuation -> exit) option ->
            match e with
            | Eio.Private.Effects.Suspend f -> Some (fun k ->
                (* Ask [f] to register whatever callbacks are needed to resume the fiber.
                   e.g. it might register a callback with a promise, for when that's resolved. *)
                f fiber (function
                    (* The fiber is ready to run again. Add it to the queue. *)
                    | Ok v -> t.run_q <- t.run_q @ [fun () -> Effect.Deep.continue k v]
                    | Error ex -> t.run_q <- t.run_q @ [fun () -> Effect.Deep.discontinue k ex]
                  );
                (* Switch to the next runnable fiber while this one's blocked. *)
                schedule t
              )
            | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
                (* Arrange for the forking fiber to run immediately after the new one. *)
                t.run_q <- Effect.Deep.continue k :: t.run_q;
                (* Create and run the new fiber (using fiber context [new_fiber]). *)
                fork ~new_fiber f
              )
            | Eio.Private.Effects.Get_context -> Some (fun k ->
                Effect.Deep.continue k fiber
              )
            | Eio.Private.Effects.Trace -> Some (fun k ->
                Effect.Deep.continue k Eio.Private.default_traceln
              )
            | _ -> None
        }
    in
    let new_fiber = Fiber_context.make_root () in
    let Exit_scheduler = fork ~new_fiber main in
    ()
end
```

It supports forking, tracing, suspending and cancellation:

```ocaml
# Eio_null.run @@ fun () ->
  let s = Eio.Stream.create 1 in
  try
    Fiber.both
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
