(*
 * Copyright (C) 2023 Thomas Leonard
 * Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Fds = Set.Make(struct
    type t = Unix.file_descr
    let compare = Stdlib.compare
  end)

type request = {
  fd : Unix.file_descr;
  direction : [ `R | `W ];
  mutable watch_connect : bool;
  finish : (unit, exn) result -> unit;
  cancelled : exn option Atomic.t;
}

type state = {
  reader : Unix.file_descr;
  writer : Unix.file_descr;
  added : request list Atomic.t;
  closed : bool Atomic.t;
}

type worker = {
  state : state;
  thread : Thread.t;
  sockets : bool;
  requests : int Atomic.t;
}

type t = worker list ref

external is_socket : Unix.file_descr -> bool = "caml_eio_windows_is_socket" [@@noalloc]

let byte = Bytes.of_string "!"

let rec notify t =
  match Unix.single_write t.writer byte 0 1 with
  | _ -> ()
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
  | exception Unix.Unix_error (EINTR, _, _) -> notify t

let run t =
  let control = Bytes.create 128 in
  let rec poll active =
    let added = Atomic.exchange t.added [] in
    if not (Atomic.get t.closed) then (
      (* Only this thread finishes a request. Once the fiber resumes, select
         can no longer be using the descriptor on its behalf. *)
      let active = List.filter (fun r ->
          match Atomic.get r.cancelled with
          | None -> true
          | Some ex -> r.finish (Error ex); false
        ) (List.rev_append added active) in
      let fds want =
        List.filter_map (fun r -> if want r then Some r.fd else None) active
        |> List.sort_uniq Stdlib.compare in
      let read = t.reader :: fds (fun r -> r.direction = `R) in
      let write = fds (fun r -> r.direction = `W) in
      let exceptional = fds (fun r -> r.watch_connect) in
      match Unix.select read write exceptional (-1.) with
      | readable, writable, errors ->
        let readable = Fds.of_list readable and writable = Fds.of_list writable in
        let errors = Fds.of_list errors in
        if Fds.mem t.reader readable then (
          try ignore (Unix.read t.reader control 0 (Bytes.length control))
          with Unix.Unix_error (EINTR, _, _) -> ()
        );
        let pending = List.filter (fun r ->
            let ready = match r.direction with
              | `R -> Fds.mem r.fd readable
              | `W ->
                Fds.mem r.fd writable ||
                (r.watch_connect && Fds.mem r.fd errors &&
                 (* Winsock also reports urgent data here. Leave SO_ERROR for the caller. *)
                 match Unix.getpeername r.fd with
                 | _ -> r.watch_connect <- false; false
                 | exception Unix.Unix_error _ -> true) in
            if ready then r.finish (match Atomic.get r.cancelled with
                | None -> Ok ()
                | Some ex -> Error ex);
            not ready
          ) active in
        poll pending
      | exception Unix.Unix_error (EINTR, _, _) -> poll active
      | exception ex ->
        (* A descriptor rejected by select must only fail its own waiters. *)
        let failed, pending = List.partition (fun r ->
            let rd, wr = match r.direction with `R -> [r.fd], [] | `W -> [], [r.fd] in
            match Unix.select rd wr wr 0. with
            | _ | exception Unix.Unix_error (EINTR, _, _) -> false
            | exception Unix.Unix_error _ -> true
          ) active in
        let failed, pending = if failed = [] then active, [] else failed, pending in
        List.iter (fun r -> r.finish (Error ex)) failed;
        poll pending
    )
  in
  poll []

let create ~sockets =
  let reader, writer = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  match
    Unix.set_nonblock writer;
    let state = { reader; writer; added = Atomic.make []; closed = Atomic.make false } in
    let requests = Atomic.make 0 in
    { state; thread = Thread.create run state; sockets; requests }
  with
  | result -> result
  | exception ex -> Unix.close reader; Unix.close writer; raise ex

module Pipe = struct
  type observation
  external is_pipe : Unix.file_descr -> bool = "caml_eio_windows_is_pipe" [@@noalloc]
  external start : Unix.file_descr -> bool -> observation = "caml_eio_windows_start_pipe_probe"
  external result : observation -> int = "caml_eio_windows_pipe_probe_result"
  external release : observation -> unit = "caml_eio_windows_release_pipe_probe"

  let await ~sleep direction fd =
    let rec poll () =
      let probe = start fd (direction = `W) in
      let rec wait () =
        match result probe with
        | -1 -> sleep 0.001; wait ()
        | n -> n <> 0
      in
      if not (Fun.protect wait ~finally:(fun () -> release probe)) then (sleep 0.01; poll ())
    in
    Eio.Fiber.check ();
    poll ()
end

let await workers direction fd finish =
  (* Unix.select uses Winsock select for socket-only sets that fit FD_SETSIZE.
     Its mixed-handle fallback replaces the event on a socket, which can lose
     a simultaneous wait on the same socket or a duplicate in another domain. *)
  let sockets = is_socket fd in
  let fits w = w.sockets = sockets && (not sockets || Atomic.get w.requests < Config.fd_setsize - 1) in
  let worker = match List.find_opt fits !workers with
    | Some w -> w
    | None ->
      let w = create ~sockets in
      workers := w :: !workers;
      w
  in
  let t = worker.state in
  let finish result = Atomic.decr worker.requests; finish result in
  let r = { fd; direction; watch_connect = (direction = `W); finish; cancelled = Atomic.make None } in
  let rec add () =
    let old = Atomic.get t.added in
    if not (Atomic.compare_and_set t.added old (r :: old)) then add ()
  in
  Atomic.incr worker.requests;
  add ();
  notify t;
  fun ex -> Atomic.set r.cancelled (Some ex); notify t

let close { state = t; thread; _ } =
  Atomic.set t.closed true;
  notify t;
  Thread.join thread;
  Unix.close t.writer;
  Unix.close t.reader

let with_worker fn =
  let workers = ref [] in
  Fun.protect (fun () -> fn workers) ~finally:(fun () -> List.iter close !workers)
