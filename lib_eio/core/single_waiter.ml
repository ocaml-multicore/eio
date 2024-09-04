type 'a state =
  | Running
  | Sleeping of (('a, exn) result -> unit)

type 'a t = 'a state ref

let create () = ref Running

let wake t v =
  match !t with
  | Running -> false
  | Sleeping fn ->
    t := Running;
    fn v;
    true

let wake_if_sleeping t =
  ignore (wake t (Ok ()) : bool)

let await t op id =
  let x =
    Suspend.enter op @@ fun ctx enqueue ->
    Cancel.Fiber_context.set_cancel_fn ctx (fun ex ->
        t := Running;
        enqueue (Error ex)
      );
    t := Sleeping (fun x ->
        Cancel.Fiber_context.clear_cancel_fn ctx;
        t := Running;
        enqueue x
      )
  in
  Trace.get id;
  x

let await_protect t op id =
  let x =
    Suspend.enter_unchecked op @@ fun _ctx enqueue ->
    t := Sleeping (fun x -> t := Running; enqueue x)
  in
  Trace.get id;
  x
