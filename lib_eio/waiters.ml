type 'a t = (('a, exn) result -> unit) Lwt_dllist.t

type waiter = unit -> unit

let create = Lwt_dllist.create

let add_waiter t cb =
  let w = Lwt_dllist.add_l cb t in
  (fun () -> Lwt_dllist.remove w)

let wake_all t v =
  try
    while true do
      Lwt_dllist.take_r t v
    done
  with Lwt_dllist.Empty -> ()

let wake_one t v =
  match Lwt_dllist.take_opt_r t with
  | None -> `Queue_empty
  | Some f -> f v; `Ok

let remove_waiter f = f ()

let is_empty t = Lwt_dllist.is_empty t
