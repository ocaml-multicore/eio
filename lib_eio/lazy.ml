open Std

type 'a state =
  | Value of 'a
  | Waiting of (unit Promise.u -> unit)
  | Running of unit Promise.t   (* Wait until resolved and check again *)
  | Failed of Exn.with_bt

type 'a t = 'a state Atomic.t

let init = Waiting (fun _ -> assert false)

let from_fun ~cancel fn =
  let state = Atomic.make init in
  let rec force r =
    match
      if cancel = `Protect then Cancel.protect fn else fn ()
    with
    | x ->
      Atomic.set state (Value x);
      Promise.resolve r ()
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      match ex with
      | Cancel.Cancelled _ when cancel = `Restart && Fiber.is_cancelled () ->
        Atomic.set state (Waiting force);
        Promise.resolve r ();
        Fiber.check ()
      | _ ->
        Atomic.set state (Failed (ex, bt));
        Promise.resolve r ();
        Printexc.raise_with_backtrace ex bt
  in
  Atomic.set state @@ Waiting force;
  state

let from_val v = Atomic.make (Value v)

let rec force t =
  match Atomic.get t with
  | Value v -> v
  | Failed (ex, bt) -> Printexc.raise_with_backtrace ex bt
  | Running p -> Promise.await p; force t
  | Waiting fn as prev ->
    let p, r = Promise.create () in
    if Atomic.compare_and_set t prev (Running p) then fn r;
    force t
