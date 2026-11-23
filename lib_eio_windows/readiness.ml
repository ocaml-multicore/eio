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
  finish : (unit, exn) result -> unit;
  cancelled : exn option Atomic.t;
}

type state = {
  reader : Unix.file_descr;
  writer : Unix.file_descr;
  added : request list Atomic.t;
  closed : bool Atomic.t;
}

type t = (state * Thread.t) Lazy.t

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
      let fds direction =
        List.filter_map (fun r -> if r.direction = direction then Some r.fd else None) active
        |> List.sort_uniq Stdlib.compare in
      let write = fds `W in
      match Unix.select (t.reader :: fds `R) write write (-1.) with
      | readable, writable, errors ->
        let readable = Fds.of_list readable and writable = Fds.of_list (writable @ errors) in
        if Fds.mem t.reader readable then (
          try ignore (Unix.read t.reader control 0 (Bytes.length control))
          with Unix.Unix_error (EINTR, _, _) -> ()
        );
        let pending = List.filter (fun r ->
            let ready = match r.direction with
              | `R -> Fds.mem r.fd readable
              | `W -> Fds.mem r.fd writable in
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

let create () =
  let reader, writer = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  match
    Unix.set_nonblock writer;
    let t = { reader; writer; added = Atomic.make []; closed = Atomic.make false } in
    t, Thread.create run t
  with
  | result -> result
  | exception ex -> Unix.close reader; Unix.close writer; raise ex

let await worker direction fd finish =
  let t, _ = Lazy.force worker in
  let r = { fd; direction; finish; cancelled = Atomic.make None } in
  let rec add () =
    let old = Atomic.get t.added in
    if not (Atomic.compare_and_set t.added old (r :: old)) then add ()
  in
  add ();
  notify t;
  fun ex -> Atomic.set r.cancelled (Some ex); notify t

let close (t, worker) =
  Atomic.set t.closed true;
  notify t;
  Thread.join worker;
  Unix.close t.writer;
  Unix.close t.reader

let with_worker fn =
  let worker = lazy (create ()) in
  Fun.protect ~finally:(fun () -> if Lazy.is_val worker then close (Lazy.force worker))
    (fun () -> fn worker)
