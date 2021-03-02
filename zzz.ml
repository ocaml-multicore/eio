(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mirage, based on Lwt_unix
 * Copyright (C) 2010,2021- Anil Madhavapeddy
 * Copyright (C) 2005-2008 Jerome Vouillon
 * Laboratoire PPS - CNRS Universite Paris Diderot
 *                    2009 Jeremie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

type sleep = {
  time : float;
  mutable canceled : bool;
  thread : (unit, unit) continuation option;
}

module SleepQueue =
  Binary_heap.Make (struct
    type t = sleep
    let compare { time = t1; _ } { time = t2; _ } =
    compare t1 t2
  end)


type t = {
 sleep_queue: SleepQueue.t;
 mutable new_sleeps: sleep list; (** Sleepers added since the last iteration of the main loop:
                                     They are not added immediately to the main sleep queue in order to
                                     prevent them from being wakeup immediately by [restart_threads]. *)
}

let init () =
  let dummy = {
    time = Unix.gettimeofday ();
    canceled = false;
    thread = None; }
  in
  let sleep_queue = SleepQueue.create ~dummy 0 in
  let new_sleeps = [] in
  { sleep_queue; new_sleeps }

let sleep t d k =
  let time = Unix.gettimeofday () +. d in
  let sleeper = { time; canceled = false; thread = k } in
  t.new_sleeps <- sleeper :: t.new_sleeps

let in_the_past t =
  t <= 0. || t <= (Unix.gettimeofday ())

let rec restart_threads ({sleep_queue;_} as st) =
  match SleepQueue.minimum sleep_queue with
  | exception Binary_heap.Empty -> ()
  | { canceled = true; _ } ->
      SleepQueue.remove sleep_queue;
      restart_threads st
  | { time; thread; _ } when in_the_past time ->
      SleepQueue.remove sleep_queue;
      (match thread with Some k -> continue k () | None -> ());
      restart_threads st
  | _ -> ()

let rec get_next_timeout ({sleep_queue; _} as st) =
  match SleepQueue.minimum sleep_queue with
  | exception Binary_heap.Empty -> None
  | { canceled = true; _ } ->
      SleepQueue.remove sleep_queue;
      get_next_timeout st
  | { time; _ } ->
      Some (time -. (Unix.gettimeofday ()))

let select_next t =
  (* Transfer all sleepers added since the last iteration to the main
     sleep queue: *)
  List.iter (fun e -> SleepQueue.add t.sleep_queue e) t.new_sleeps;
  t.new_sleeps <- [];
  get_next_timeout t
