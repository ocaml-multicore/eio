(* Asynchronous IO is achieved thanks to GCD's dispatch_io functions. 
   The implementation is largely based on the Eunix counterpart. *)
  
let src = Logs.Src.create "edispatch" ~doc:"Effect-based IO system using GCD"
module Log = (val Logs.src_log src : Logs.LOG)

(* open Fibreslib *)

type amount = Exactly of int | Upto of int

let system_thread = Ctf.mint_id ()

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

type data = int Suspended.t 

(* State *)
type t = {
  dqueue : data Dqueue.t
}

(* IO Functions *)
let enqueue_read { dqueue } action (file_offset, fd, buff, len) =
  let len = match len with Upto l -> l | Exactly l -> l in 
  let req = { Dqueue.req = `R; fd = FD.to_unix fd; file_offset; len; id = 1; data = action; buff } in
  Log.debug (fun l -> l "read: submitting call");
  Ctf.label "read";
  Dqueue.enqueue dqueue req

let enqueue_write { dqueue } action (file_offset, fd, buff, len) =
  let len = match len with Upto l -> l | Exactly l -> l in 
  let req = { Dqueue.req = `W; fd = FD.to_unix fd; file_offset; len; id = 1; data = action; buff } in
  Log.debug (fun l -> l "write: submitting call");
  Ctf.label "write";
  Dqueue.enqueue dqueue req

(* IO Effects *)
effect ERead : (int * FD.t * Cstruct.t * amount) -> int

let read_upto ~file_offset fd buf len =
  let res = perform (ERead (file_offset, fd, buf, Upto len)) in
  Log.debug (fun l -> l "read_upto: woken up after read");
  if res < 0 then
    failwith "read_upto failed"
  else
    res

effect EWrite : (int * FD.t * Cstruct.t * amount) -> int

let write ~file_offset fd buf len =
  let res = perform (EWrite (file_offset, fd, buf, Exactly len)) in
  Log.debug (fun l -> l "writing: woken up after read");
  if res < 0 then
    failwith "write failed"

module Objects = struct
  type _ Eio.Generic.ty += FD : FD.t Eio.Generic.ty

  class source fd = object
    inherit Eio.Source.t as super

    method fd = fd
    method close = FD.close fd

    method! probe : type a. a Eio.Generic.ty -> a option = function
      | FD -> Some fd
      | x -> super#probe x

    method read_into buf =
      let chunk = Cstruct.create 1024 in 
      let got = read_upto ~file_offset:0 fd chunk 1024 in
      Cstruct.blit chunk 0 buf 0 got;
      got
  end

  class sink fd = object
    inherit Eio.Sink.t

    method fd = fd
    method close = FD.close fd

    (* 
      TODO(patricoferris): probably can do a fast-copy 
      from dispatch_data_t to dispatch_data_t :) 
    *)

    method write src = 
      let chunk = Cstruct.create 1024 in 
      try
        while true do
          let got = Eio.Source.read_into src chunk in
          write ~file_offset:0 fd chunk got
        done
      with End_of_file -> ()
  end

  let stdenv () =
    let stdin = lazy (new source (FD.of_unix Unix.stdin)) in
    let stdout = lazy (new sink (FD.of_unix Unix.stdout)) in
    let stderr = lazy (new sink (FD.of_unix Unix.stderr)) in
    object (_ : Eio.Stdenv.t)
      method stdin = (Lazy.force stdin :> Eio.Source.t)
      method stdout = (Lazy.force stdout :> Eio.Sink.t)
      method stderr = (Lazy.force stderr :> Eio.Sink.t)
    end
end

let schedule ({ dqueue } as _st) = 
  let _ = Dqueue.submit dqueue in 
  let res = Dqueue.wait dqueue in 
    match res with 
      | None -> `Exit_scheduler
      (* TODO: Something here... *)
      | Some { data; id; _ } -> Suspended.continue data id

let run main =
  Log.debug (fun l -> l "starting run");
  let stdenv = Objects.stdenv () in
  let dqueue = Dqueue.create () in 
  let st = { dqueue } in
  Log.debug (fun l -> l "starting main thread");
  let fork ~tid fn =
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
  in
  let main_done = ref false in
  let `Exit_scheduler = fork ~tid:(Ctf.mint_id ()) (fun () ->
      Fun.protect (fun () -> main stdenv)
        ~finally:(fun () -> main_done := true)
  ) in
  if not !main_done then
    failwith "Deadlock detected: no events scheduled but main function hasn't returned";
  Log.debug (fun l -> l "exit")