(* A read/write request and completion queue inspired by io_uring *)
type 'a request = {
  req : [`R | `W];
  fd : Unix.file_descr;
  file_offset : int; 
  len : int;
  id : int;
  buff : Cstruct.t;
  data : 'a;
}

type 'a completion = {
  id : int;
  group : Dispatch.Group.t;
  data : 'a;
}

type 'a t = ('a request Queue.t * 'a completion Queue.t)

let create () = (Queue.create (), Queue.create ())

let read_write_queue = Dispatch.Queue.(create ~typ:Serial ())

let enqueue (reqs, _) req = Queue.push req reqs

let submit (reqs, compls) = 
  try 
    while true do 
      match Queue.pop reqs with 
        | { req = `R; fd; file_offset=_; len=_; id; data; _ } ->  
          let read_io = Dispatch.Io.(create Stream fd read_write_queue) in
          let group = Dispatch.Group.create () in
          Queue.push {id; data; group} compls;
          Dispatch.Io.with_read read_write_queue read_io group
            ~f:(fun _data -> () (*TODO: Copy data to cstruct thing or something... *))
            ~err:(fun () -> print_endline "Oh no!" (* Do something serious here... *))
        | { req = `W; fd; file_offset=_; len=_; id; data; buff } ->
          let group = Dispatch.Group.create () in 
          let write_io = Dispatch.Io.(create Stream fd read_write_queue) in
          Queue.push {id; data; group} compls;
          Dispatch.Io.write read_write_queue write_io 0 group (Dispatch.Data.create (buff |> Cstruct.to_bigarray))
    done 
  with Queue.Empty -> ()

let wait ?(timeout=Dispatch.Time.forever) (_, compls) = 
  try 
    let ( { group; _ } as compl) = Queue.pop compls in 
      Dispatch.(Group.wait group @@ timeout ()) |> ignore;
      Some compl 
  with Queue.Empty -> None 