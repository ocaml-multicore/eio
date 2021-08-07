(* A read/write request and completion queue inspired by io_uring *)
type 'a rw = {
  req : [ `R | `W ];
  fd : Unix.file_descr;
  file_offset : int;
  len : int;
  id : int;
  buff : Dispatch.Data.t ref;
  data : 'a;
}

type 'a request = Close of Unix.file_descr * 'a | Rw of 'a rw

type eof = Eof | Eof_with_data | Not_eof

type 'a completion = { id : int; data : 'a; eof : eof; bytes_read : int }

type 'a t = {
  request_q : 'a request Queue.t;
  pending_io : int ref;
  completion_q : 'a completion Queue.t;
  rw_q : Dispatch.Queue.t;
}

let create () =
  {
    request_q = Queue.create ();
    pending_io = ref 0;
    completion_q = Queue.create ();
    rw_q = Dispatch.Queue.(create ~typ:Serial ());
  }

let enqueue { request_q; _ } req = Queue.push req request_q

let submit { request_q = reqs; pending_io; completion_q = compls; rw_q = read_write_queue } =
  let jobs = ref 0 in
  try
    while true do
      match Queue.pop reqs with
      | Rw ({ req = `R; fd; id; data; buff; len; _ } as request) ->
          incr pending_io;
          incr jobs;
          let read_io = Dispatch.Io.(create Stream fd read_write_queue) in
          let bytes_read = ref 0 in 
          Dispatch.Io.with_read ~queue:read_write_queue ~off:0 ~length:len
            ~f:(fun ~err ~finished read ->
              if Dispatch.Data.size read = 0 && err = 0 then (
                if !bytes_read <> 0 then 
                  Queue.push { id; data; eof = Eof_with_data; bytes_read = !bytes_read } compls
                else
                  Queue.push { id; data; eof = Eof; bytes_read = !bytes_read} compls
              )
              else (
                if Dispatch.Data.size read > 0 then (
                  bytes_read := !bytes_read + Dispatch.Data.size read;
                  request.buff := Dispatch.Data.concat !buff read );
                if finished then Queue.push { id; data; eof = Not_eof; bytes_read = !bytes_read } compls ))
            read_io
      | Rw { req = `W; fd; file_offset; len; id; data; buff } ->
          incr pending_io;
          incr jobs;
          let write_io = Dispatch.Io.(create Stream fd read_write_queue) in
          let write_data = Dispatch.Data.sub file_offset len !buff in 
          Dispatch.Io.with_write ~queue:read_write_queue ~off:0 ~data:write_data
            ~f:(fun ~err:_ ~finished _ ->
              if finished then Queue.push { id; data; eof = Not_eof; bytes_read = 0 } compls)
            write_io
      | Close (fd, data) ->
          incr pending_io;
          incr jobs;
          Dispatch.async read_write_queue (fun () ->
              Unix.close fd;
              Queue.push { id = 0; data; eof = Not_eof; bytes_read = 0 } compls)
    done;
    !jobs
  with Queue.Empty -> !jobs

let wait ?(timeout = Dispatch.Time.forever)
    { request_q = _reqs; pending_io; completion_q = compls; _ } =
  ignore timeout;
  let group = Dispatch.Group.create () in
  let rec loop () =
    try
      Dispatch.Group.wait group (Dispatch.Time.forever ()) |> ignore;
      (* TODO: Replace with a [Dispatch.select ()] *)
      let ({ id; _ } as compl) = Queue.pop compls in
      decr pending_io;
      Logs.debug (fun f -> f "waiting for io request %i" id);
      Some compl
    with Queue.Empty -> if !pending_io = 0 then None else loop ()
  in
  loop ()