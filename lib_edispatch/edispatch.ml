(* Asynchronous IO is achieved thanks to GCD's dispatch_io functions. 
   The implementation is largely based on the Eunix counterpart. *)
  
let src = Logs.Src.create "edispatch" ~doc:"Effect-based IO system using GCD"
module Log = (val Logs.src_log src : Logs.LOG)

open Fibreslib

type amount = Exactly of int | Upto of int

(* let system_thread = Ctf.mint_id () *)

effect Close : Unix.file_descr -> int

module FD = struct
  type t = {
    mutable fd : [`Open of Unix.file_descr | `Closed]
  }

  let get op = function
    | { fd = `Open fd } -> fd
    | { fd = `Closed } -> invalid_arg (op ^ ": file descriptor used after calling close!")

  let of_unix fd = { fd = `Open fd }
  let to_unix = get "to_unix"

  let is_open = function
    | { fd = `Open _ } -> true
    | { fd = `Closed } -> false

  let close t =
    Ctf.label "close";
    let fd = get "close" t in
    t.fd <- `Closed;
    let res = perform (Close fd) in
    Log.debug (fun l -> l "close: woken up");
    if res < 0 then
      failwith "Dispatch file closing error"
end

let openfile path flags mode =
  FD.of_unix (Unix.openfile path flags mode)

(* User data handed back in the completion queue *)
type data = 
  | Read of  (int * int Suspended.t)
  | Write of int Suspended.t
  | Close of int Suspended.t 

type runnable =
  | Thread : 'a Suspended.t * 'a -> runnable
  | Failed_thread : 'a Suspended.t * exn -> runnable

(* State *)
type t = {
  run_q : runnable Queue.t; (* For handling fibres *)
  dqueue : data Dqueue.t;   (* For handling IO *)
  mutable io_jobs : int     (* For counting IO tasks *)
}

(* IO Functions *)
let enqueue_read { dqueue; _ } action (file_offset, fd, buff, len) =
  let len = match len with Upto l -> l | Exactly l -> l in 
  let req = Dqueue.(Rw { req = `R; fd = FD.to_unix fd; file_offset; len; id = 1; data = (Read (len, action)); buff }) in
  Log.debug (fun l -> l "read: submitting call");
  Ctf.label "read";
  Dqueue.enqueue dqueue req

let enqueue_write { dqueue; _ } action (file_offset, fd, buff, len) =
  let len = match len with Upto l -> l | Exactly l -> l in 
  let req = Dqueue.(Rw { req = `W; fd = FD.to_unix fd; file_offset; len; id = 2; data = (Write action); buff }) in
  Log.debug (fun l -> l "write: submitting call");
  Ctf.label "write";
  Dqueue.enqueue dqueue req

let enqueue_thread { run_q; _ } k x =
  Queue.push (Thread (k, x)) run_q

let enqueue_failed_thread { run_q; _ } k ex =
  Queue.push (Failed_thread (k, ex)) run_q

let enqueue_close { dqueue; _ } action fd =
  Log.debug (fun l -> l "closing fd");
  let req = Dqueue.Close (fd, (Close action)) in 
  Ctf.label "close";
  Dqueue.enqueue dqueue req

(* IO Effects *)
effect ERead : (int * FD.t * Dispatch.Data.t ref * amount) -> int

let read_upto ~file_offset fd buf len =
  let res = perform (ERead (file_offset, fd, buf, Upto len)) in
  Log.debug (fun l -> l "read_upto: woken up after read");
  if res < 0 then
    failwith "read_upto failed"
  else
    res

effect EWrite : (int * FD.t * Dispatch.Data.t ref * amount) -> int

let write ~file_offset fd buf len =
  let res = perform (EWrite (file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "writing: woken up after read");
  if res < 0 then
    failwith "write failed"

    (* 
      TODO(patricoferris): probably can do a fast-copy 
      from dispatch_data_t to dispatch_data_t :) 
    *)

let fast_copy src dst = 
  let data = ref @@ Dispatch.Data.empty () in 
  try
    while true do 
      let got = read_upto ~file_offset:0 src data 1024 in
        write ~file_offset:0 dst data got
    done
  with End_of_file -> ()

module Objects = struct
  type _ Eio.Generic.ty += FD : FD.t Eio.Generic.ty

  type source = < Eio.Flow.source; Eio.Flow.close; fd : FD.t >
  type sink   = < Eio.Flow.sink  ; Eio.Flow.close; fd : FD.t >

  let flow fd = object (_ : <source; sink; ..>)

    method fd = fd
    method close = FD.close fd

    method probe : type a. a Eio.Generic.ty -> a option = function
      | FD -> Some fd
      | _ -> None

    method read_into buf =
      let data = ref (Dispatch.Data.empty ()) in 
      let got = read_upto ~file_offset:0 fd data (Cstruct.length buf) in
      let cs = Cstruct.of_bigarray @@ Dispatch.Data.to_buff ~offset:0 got !data in
      Cstruct.blit cs 0 buf 0 got;
      Dispatch.Data.size !data

    method write src = 
      match Eio.Generic.probe src FD with
      | Some src -> fast_copy src fd
      | None ->
        let chunk = Cstruct.create 1024 in 
        try
          while true do
            let got = Eio.Flow.read_into src chunk in
            write ~file_offset:0 fd (ref @@ Dispatch.Data.create (Cstruct.to_bigarray chunk)) got
          done
        with End_of_file -> ()
  end

  let source fd = (flow fd :> source)
  let sink   fd = (flow fd :> sink)

  type stdenv = <
    stdin  : source;
    stdout : sink;
    stderr : sink;
  >

  let stdenv () =
    let stdin = lazy (source (FD.of_unix Unix.stdin)) in
    let stdout = lazy (sink (FD.of_unix Unix.stdout)) in
    let stderr = lazy (sink (FD.of_unix Unix.stderr)) in
    object (_ : stdenv)
      method stdin = Lazy.force stdin
      method stdout = Lazy.force stdout
      method stderr = Lazy.force stderr
   end
end

let rec schedule ({ dqueue; run_q; _ } as st) =
  match Queue.take run_q with
  | Thread (k, v) -> Suspended.continue k v               (* We already have a runnable task *)
  | Failed_thread (k, ex) -> Suspended.discontinue k ex
  | exception Queue.Empty -> 
    let num_jobs = Dqueue.submit dqueue in 
    Log.debug (fun f -> f "Submitted %i jobs" num_jobs);
    st.io_jobs <- st.io_jobs + num_jobs;
    let res = Dqueue.wait dqueue in 
      match res with 
        | None -> 
          Log.debug (fun f -> f "No result");
          if st.io_jobs = 0 then `Exit_scheduler else schedule st
        | Some { data; id; eof; bytes_read } -> 
          st.io_jobs <- st.io_jobs - 1;
          Log.debug (fun f -> f "Result of waiting %i" id);
          match data with 
           | Read (_, data) -> (
              match eof with 
                | Eof -> Suspended.discontinue data End_of_file
                | Not_eof -> Suspended.continue data bytes_read
                | Eof_with_data -> Suspended.continue data bytes_read
           )
           | Write data -> Suspended.continue data id
           | Close data -> Suspended.continue data id

let pipe () =
  let r, w = Unix.pipe () in
  let r = Objects.source (FD.of_unix r) in
  let w = Objects.sink (FD.of_unix w) in
  r, w

let run main =
  Log.debug (fun l -> l "starting run");
  let stdenv = Objects.stdenv () in
  let dqueue = Dqueue.create () in 
  let run_q = Queue.create () in
  let st = { dqueue; run_q; io_jobs = 0 } in
  Log.debug (fun l -> l "starting main thread");
  let rec fork ~tid fn =
    Ctf.note_switch tid;
    match fn () with
    | () ->
      schedule st
    | effect (ERead args) k ->
      let k = { Suspended.k; tid } in
      enqueue_read st k args;
      schedule st
    | effect (EWrite args) k ->
      let k = { Suspended.k; tid } in
      enqueue_write st k args;
      schedule st
    | effect (Close fd) k ->
      let k = { Suspended.k; tid } in
      enqueue_close st k fd;
      schedule st
      (* Suspended.continue k 0 *)
    | effect Fibre_impl.Effects.Yield k ->
      let k = { Suspended.k; tid } in
      enqueue_thread st k ();
      schedule st
    | effect (Fibre_impl.Effects.Await (sw, pid, q)) k ->
      let k = { Suspended.k; tid } in
      let waiters = Queue.create () in
      let when_resolved r =
        Queue.iter Fibre_impl.Waiters.remove_waiter waiters;
        match r with
        | Ok v ->
          Ctf.note_read ~reader:tid pid;
          enqueue_thread st k v
        | Error ex ->
          Ctf.note_read ~reader:tid pid;
          enqueue_failed_thread st k ex
      in
      let cancel ex = when_resolved (Error ex) in
      sw |> Option.iter (fun sw ->
          let cancel_waiter = Fibre_impl.Switch.add_cancel_hook sw cancel in
          Queue.add cancel_waiter waiters;
        );
      let resolved_waiter = Fibre_impl.Waiters.add_waiter q when_resolved in
      Queue.add resolved_waiter waiters;
      schedule st
    | effect (Fibre_impl.Effects.Fork (sw, exn_turn_off, f)) k ->
      let k = { Suspended.k; tid } in
      let id = Ctf.mint_id () in
      Ctf.note_created id Ctf.Task;
      let promise, resolver = Promise.create_with_id id in
      enqueue_thread st k promise;
      fork
        ~tid:id
        (fun () ->
           Fibre_impl.Switch.with_op sw @@ fun () ->
           match f () with
           | x -> 
            Log.debug (fun f -> f "Fullfilling fork");
            Promise.fulfill resolver x
           | exception ex ->
             Log.debug (fun f -> f "Forked fibre failed: %a" Fmt.exn ex);
             if exn_turn_off then Switch.turn_off sw ex;
             Promise.break resolver ex
        )
    | effect (Fibre_impl.Effects.Fork_ignore (sw, f)) k ->
      let k = { Suspended.k; tid } in
      enqueue_thread st k ();
      let child = Ctf.note_fork () in
      Ctf.note_switch child;
      fork ~tid:child (fun () ->
          match Fibre_impl.Switch.with_op sw f with
          | () ->
            Ctf.note_resolved child ~ex:None
          | exception ex ->
            Switch.turn_off sw ex;
            Ctf.note_resolved child ~ex:(Some ex)
        )
  in
  let main_done = ref false in
  let `Exit_scheduler = fork ~tid:(Ctf.mint_id ()) (fun () ->
      Fun.protect (fun () -> main stdenv)
        ~finally:(fun () -> main_done := true)
  ) in
  if not !main_done then
    failwith "Deadlock detected: no events scheduled but main function hasn't returned";
  Log.debug (fun l -> l "exit")