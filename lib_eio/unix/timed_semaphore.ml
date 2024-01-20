(* This file is mostly taken from OCaml PR#12867.
 * It exposes pthread_cond_timedwait(3) for timed_semaphore.ml.
 * This file (and thread_pool.c) can both be deleted once
 * this feature is available in OCaml itself. *)

external condition_timed_wait : Condition.t -> Mutex.t -> float -> bool = "eio_unix_condition_timedwait"

type t = {
  mut: Mutex.t;                         (* protects [v] *)
  mutable v: int;                       (* the current value *)
  nonzero: Condition.t                  (* signaled when [v > 0] *)
}

let make b =
  {
    mut = Mutex.create();
    v = if b then 1 else 0;
    nonzero = Condition.create();
  }

let release s =
  Mutex.lock s.mut;
  s.v <- 1;
  Condition.signal s.nonzero;
  Mutex.unlock s.mut

let acquire s =
  Mutex.lock s.mut;
  while s.v = 0 do Condition.wait s.nonzero s.mut done;
  s.v <- 0;
  Mutex.unlock s.mut

let acquire_with_timeout s timeout =
  Mutex.lock s.mut;
  let rec aux () =
    if s.v = 0 then begin
      let signaled = condition_timed_wait s.nonzero s.mut timeout in
      if signaled && s.v = 0
      then aux ()
      else signaled
    end
    else true
  in
  let signaled = aux () in
  s.v <- 0;
  Mutex.unlock s.mut;
  signaled

let try_acquire s =
  Mutex.lock s.mut;
  let ret = if s.v = 0 then false else (s.v <- 0; true) in
  Mutex.unlock s.mut;
  ret
