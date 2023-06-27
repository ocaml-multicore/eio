type t =
  | Null
  | Node : 'a Lwt_dllist.node -> t
  | Node_with_mutex : 'a Lwt_dllist.node * Mutex.t -> t

let null = Null

let remove = function
  | Null -> ()
  | Node n -> Lwt_dllist.remove n
  | Node_with_mutex (n, m) ->
    Mutex.lock m;
    Fun.protect ~finally:(fun () -> Mutex.unlock m)
      (fun () -> Lwt_dllist.remove n)
