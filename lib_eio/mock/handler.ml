type 'a actions = 'a Action.t list

type 'a t = {
  default_action : 'a Action.t;
  mutable handler : (unit -> 'a);
}

let run t = t.handler ()

let set_handler t f = t.handler <- f

let seq t actions =
  let actions = ref actions in
  let next () =
    match !actions with
    | [] -> Action.run t.default_action
    | x :: xs ->
      actions := xs;
      Action.run x
  in
  set_handler t next

let run_default_action t =
  Action.run t.default_action

let make default_action =
  { default_action; handler = (fun () -> Action.run default_action) }
