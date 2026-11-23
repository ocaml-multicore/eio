(* Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: ISC *)

open Eio.Std

external current_thread_id : unit -> int32 = "caml_eio_windows_io_thread_id"
external cancel : int32 -> bool = "caml_eio_windows_cancel_sync_io"

let run_with_check fn =
  Switch.run @@ fun sw ->
  let lock = Mutex.create () in
  let active = ref None and cancelled = ref None in
  let result = Fiber.fork_promise ~sw (fun () ->
      Eio_unix.run_in_systhread (fun () ->
          let id = current_thread_id () in
          Fun.protect ~finally:(fun () -> Mutex.protect lock (fun () -> active := None))
            (fun () ->
               Mutex.protect lock (fun () -> Option.iter raise !cancelled; active := Some id);
               fn (fun () -> Mutex.protect lock (fun () -> Option.iter raise !cancelled))))) in
  match Promise.await result with
  | Ok n -> n
  | Error ex -> raise ex
  | exception ex ->
    Eio.Cancel.protect @@ fun () ->
    (* A published worker may still be about to enter WriteFile. Its ID is
       retired under the same lock before the pool can assign it another job. *)
    let stop () =
      cancelled := Some ex;
      match !active with None -> true | Some id -> cancel id
    in
    let rec interrupt () =
      if not (Mutex.protect lock stop) then (Fiber.yield (); interrupt ())
    in
    interrupt ();
    match Promise.await result with
    | Ok n -> n
    | Error _ -> raise ex

let run fn = run_with_check (fun _ -> fn ())

external pipe_write_size : Unix.file_descr -> int -> int = "caml_eio_windows_pipe_write_size"

let write fd (buf : Cstruct.t) =
  run_with_check (fun check ->
      let len = pipe_write_size fd buf.len in
      check ();
      Unix.single_write_bigarray fd buf.buffer buf.off len)
