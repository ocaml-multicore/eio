open Eio.Std

let mbox = Eio.Mbox.create ()

let total_sent = Atomic.make 0

let sleepy_time ~clock =
  Eio.Time.sleep clock @@
  Random.float 0.5

let sender ~clock s () =
  while true do
    Eio.Mbox.send mbox s;
    Atomic.incr total_sent;
    sleepy_time ~clock
  done

let receiver nmsg ~clock () =
  let rec loop n =
    if n = nmsg then
      begin
        while Atomic.get total_sent < nmsg do
          Domain.cpu_relax ()
        done;
        traceln "received %d messages, total_sent is %d (-1)"
          n (Atomic.get total_sent)
      end
    else
      let m = Eio.Mbox.recv mbox in
      traceln "%d\t%s" n m;
      sleepy_time ~clock;
      loop (succ n)
  in
  loop 0

let main nmsg ~domain_mgr ~clock =
  let domain_fork (f: unit -> unit) () = Eio.Domain_manager.run domain_mgr f in

  Switch.run @@ fun _sw ->
  let nsenders = max (pred Domain.recommended_domain_count) 1 in
  let senders = List.init nsenders @@ fun idx ->
    domain_fork (sender ~clock (Printf.sprintf "msg from sender %d" idx))
  in
  let receiver = domain_fork (receiver nmsg ~clock) in
  Fiber.any (receiver :: senders)

let () =
  Eio_main.run @@ fun env ->
  let nmsg = try Sys.argv.(1) |> int_of_string with Invalid_argument _ -> 100 in
  main nmsg ~domain_mgr:(Eio.Stdenv.domain_mgr env) ~clock:(Eio.Stdenv.clock env)
