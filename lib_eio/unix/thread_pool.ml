(** This thread pool does not spawn threads in advance,
   but up to [max_standby_systhreads_per_domain] threads are
   kept alive to wait for more work to arrive.
   This number was chosen somewhat arbitrarily but benchmarking
   shows it to be a good compromise.
   Warning: do not increase this number above 32. *)
let max_standby_systhreads_per_domain = 20

(** After completing a job, each thread will wait [max_wait_seconds]
    for new work to arrive. If it does not receive work within this
    period of time, the worker thread exits. *)
let max_wait_seconds = 0.2

module Bitfield : sig
  type t
  val create : unit -> t
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
  val is_empty : t -> bool
  val is_full : t -> bool
  val find_ready : t -> int
  val find_empty : t -> int
end = struct
  type t = int ref

  let create () = ref 0

  let[@inline always] get t i = !t land (1 lsl i) <> 0

  let[@inline always] set t i = function
  (* cannot do an inline if without allocating, but this pattern match is okay *)
  | true -> t := !t lor (1 lsl i)
  | false -> t := !t lxor (1 lsl i)

  let[@inline always] is_empty t = !t = 0

  let full = (1 lsl max_standby_systhreads_per_domain) - 1

  let[@inline always] is_full t = !t = full

  (** fls: Find Last Set (bit)
      Returns the 0-indexed position of the least significant set bit in [v] *)
  let[@inline always] fls v =
    (* tz: trailing zeroes in [v] become ones, everything else is blanked out *)
    let tz = lnot v land (v - 1) in
    (* popcount() *)
    let x = (tz land 0x55555555) + ((tz lsr 1) land 0x55555555) in
    let x = (x land 0x33333333) + ((x lsr 2) land 0x33333333) in
    let x = (x land 0x0F0F0F0F) + ((x lsr 4) land 0x0F0F0F0F) in
    let x = (x land 0x00FF00FF) + ((x lsr 8) land 0x00FF00FF) in
    (x land 0x0000FFFF) + ((x lsr 16) land 0x0000FFFF)

  (** Finds least significant 1 *)
  let[@inline always] find_ready t = fls !t

  (** Finds least significant 0 *)
  let[@inline always] find_empty t = (lnot !t) land full |> fls
end

type job =
| New
| Exit
| Job : {
  fn: unit -> 'a;
  enqueue: ('a, exn) result -> unit;
} -> job

(** This threadpool record type looks the way it does due to the constraints
   imposed onto it by the need to run certain bookkeeping operations inside of
   [@poll error] functions.

   Note: type ['a] will be [Mailbox.t], defined below. *)
type 'a t = {
  mutable initialized: bool; (* Have we setup the [Domain.at_exit] callback? *)
  threads: 'a array; (* An array of [Mailbox.t] *)
  available: Bitfield.t; (* For each thread, is it ready to receive work? *)
  (* mutable n_available: int; (* Number of threads waiting for work *) *)
  mutable terminating: bool; (* Is the domain exiting? *)
}

module Mailbox = struct
  (** Mailbox with blocking OS semaphore.
     Each [Mailbox.t] controls one worker systhread.
     After completing each job, a thread will be kept for a short period
     of time in case more work is immediately available.
     Otherwise, the thread exits. *)
  type t = {
    lock: Timed_semaphore.t; (* A Unix [pthread_cond_t] *)
    mutable cell: job;
    mutable i: int; (* The index of this thread in the threadpool arrays *)
  }

  (* A worker systhread does not start with an assigned index.
     [i] cannot be an option because writing a Some to this field would allocate. *)
  let create () = { lock = Timed_semaphore.make false; cell = New; i = -1 }

  let dummy = create ()

  let put mbox x =
    mbox.cell <- x;
    Timed_semaphore.release mbox.lock

  (* [@poll error] ensures this function is atomic within systhreads of a single domain.
     Returns [true] if the thread should exit. *)
  let[@poll error] handle_timedout pool = function
  | { i = -1; _} ->
    (* This thread was never placed in the pool, there is nothing to clean up *)
    true
  | { i; _ } ->
    match Bitfield.get pool.available i with
    | true ->
      (* Cleanup and exit *)
      Bitfield.set pool.available i false;
      true
    | false ->
      (* A thread switch happened right before the start of this function.
         [Timed_semaphore.acquire_with_timeout] timed out, but a new job came in
         for this thread before we could stop the thread, so execute the new job. *)
      false

  let rec take pool mbox =
    if Timed_semaphore.acquire_with_timeout mbox.lock max_wait_seconds
    then mbox.cell
    else (
      (* Semaphore timed out *)
      if handle_timedout pool mbox
      then Exit
      else take pool mbox
    )
end

let create () =
  {
    initialized = false;
    threads = Array.make max_standby_systhreads_per_domain Mailbox.dummy;
    available = Bitfield.create ();
    terminating = false;
  }

(* This function is necessary in order to immediately terminate the systhreads
   on stand-by, without having to wait for them to shut down automatically.
   Without this termination mechanism, domains that have executed a call on
   a systhread in the last [max_wait_seconds] seconds would have to wait for
   their timeout to occur, introducing long unwanted pauses. *)
let terminate pool =
  pool.terminating <- true;
  if not (Bitfield.is_empty pool.available) then
    for i = 0 to max_standby_systhreads_per_domain - 1 do
      if Bitfield.get pool.available i
      then Mailbox.put (Array.unsafe_get pool.threads i) Exit
    done

(* [@poll error] ensures this function is atomic within systhreads of a single domain.
   This function (re-)adds the worker systhread to the pool of available threads,
   or exits if the pool is already at maximum capacity. *)
let[@poll error] keep_thread_or_exit pool (mbox : Mailbox.t) =
  if pool.terminating || Bitfield.is_full pool.available
  then raise Thread.Exit
  else (
    let i = Bitfield.find_empty pool.available in
    mbox.i <- i;
    Bitfield.set pool.available i true;
    Array.unsafe_set pool.threads i mbox
  )

(* [@poll error] ensures this function is atomic within systhreads of a single domain.
   This function tries to reserve one available thread from the pool.
   Since we cannot return an option in [@poll error] code, we simulate
   an option by conditionally updating the reference [res].
   The returned boolean indicates whether we updated [res] or not. *)
let[@poll error] try_get_thread (pool : Mailbox.t t) res =
  if Bitfield.is_empty pool.available
  then false
  else (
    let i = Bitfield.find_ready pool.available in
    Bitfield.set pool.available i false;
    res := Array.unsafe_get pool.threads i;
    true)

let make_thread pool =
  let mbox = Mailbox.create () in
  let _t : Thread.t = Thread.create (fun () ->
    while true do
      match Mailbox.take pool mbox with
      | New -> assert false
      | Exit -> raise Thread.Exit
      | Job { fn; enqueue } ->
        enqueue (try Ok (fn ()) with exn -> Error exn);
        keep_thread_or_exit pool mbox
    done
  ) ()
  in
  mbox

(* https://v2.ocaml.org/manual/parallelism.html#s:par_systhread_interaction
   "Only one systhread at a time is allowed to run OCaml code on a particular domain."
   So we keep a separate threadpool per domain. *)
let key = Domain.DLS.new_key create

(* [@poll error] ensures this function is atomic within systhreads of a single domain.
   https://github.com/ocaml/ocaml/pull/12724
   As of OCaml 5.1, [Domain.at_exit] is still not threadsafe *)
let[@poll error] needs_init pool =
  if pool.initialized
  then false
  else (pool.initialized <- true; true)

let run_on_systhread ~enqueue fn =
  let pool = Domain.DLS.get key in
  if needs_init pool then Domain.at_exit (fun () -> terminate pool);
  let mbox =
    let res = ref Mailbox.dummy in
    if try_get_thread pool res
    then !res
    else make_thread pool
  in
  Mailbox.put mbox (Job { fn; enqueue })
