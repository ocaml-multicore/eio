(* This thread pool does not spawn threads in advance,
   but up to [max_standby_systhreads_per_domain] threads are
   kept alive to wait for more work to arrive.
   This number was chosen somewhat arbitrarily but benchmarking
   shows it to be a good compromise.
   Warning: do not update this number without updating
   [find_thread_predicate_exists] below. *)
let max_standby_systhreads_per_domain = 20

type job =
  | New
  | Exit
  | Job : {
      fn: unit -> 'a;
      enqueue: ('a, exn) result -> unit;
    } -> job

exception Assertion_failure

type 'a t = {
  mutable initialized: bool;
  threads: 'a array;
  available: bool array;
  mutable n_available: int;
  mutable terminating: bool;
}

(* Mailbox with blocking semaphore *)
module Mailbox = struct
  type t = {
    lock: Timed_semaphore.t;
    mutable cell: job;
    mutable i: int;
  }

  let create () = { lock = Timed_semaphore.make false; cell = New; i = -1 }

  let dummy = create ()

  let put mbox x =
    mbox.cell <- x;
    Timed_semaphore.release mbox.lock

  (* [@poll error] makes this function atomic from the point of view
     of threads within a single domain. *)
  let[@poll error] handle_timedout pool = function
    | { i = -1; _} ->
      (* This thread was never placed in the pool, just exit *)
      true
    | { i;_ } ->
      match Array.unsafe_get pool.available i with
      | true ->
        (* Cleanup and exit *)
        Array.unsafe_set pool.available i false;
        pool.n_available <- pool.n_available - 1;
        true
      | false ->
        (* A thread switch happened right before the start of this function.
           [Timed_semaphore.acquire_with_timeout] timed out, but a new job came in
           for this thread before we could stop the thread, so execute the new job. *)
        false

  let rec take pool mbox =
    if Timed_semaphore.acquire_with_timeout mbox.lock 5.0
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
    threads = Array.init max_standby_systhreads_per_domain (fun _ -> Mailbox.dummy);
    available = Array.init max_standby_systhreads_per_domain (fun _ -> false);
    n_available = 0;
    terminating = false;
  }

(* This function is necessary in order to immediately terminate the systhreads
   on stand-by, without having to wait for them to shut down automatically.
   Without this termination mechanism, domains that have executed a call on
   a systhread in the last 0.2 seconds will have to wait for their timeout to
   occur, introducing long unwanted pauses. *)
let terminate pool =
  pool.terminating <- true;
  if pool.n_available > 0 then
    for i = 0 to max_standby_systhreads_per_domain - 1 do
      if Array.unsafe_get pool.available i
      then Mailbox.put (Array.unsafe_get pool.threads i) Exit
    done

(* This function is called from within a [@poll error] function,
   so we cannot allocate, call OCaml functions, or use for/while loops.
   See https://discuss.ocaml.org/t/using-poll-error-attribute-to-implement-systhread-safe-data-structures/12804
   The classic CAS retry loop is also out because it was observed
   to cause extreme levels of contention.
   The only option left to us is to unroll the loop by hand.
   It is marked [@inline always] to make it usable from [@poll error] code.
   This function finds the first thread in the desired state [b].
   As the name implies, at least one thread must be known to be in state [b]. *)
let[@inline always] find_thread_predicate_exists pool b =
  if Array.unsafe_get pool.available 0 = b then 0 else
  if Array.unsafe_get pool.available 1 = b then 1 else
  if Array.unsafe_get pool.available 2 = b then 2 else
  if Array.unsafe_get pool.available 3 = b then 3 else
  if Array.unsafe_get pool.available 4 = b then 4 else
  if Array.unsafe_get pool.available 5 = b then 5 else
  if Array.unsafe_get pool.available 6 = b then 6 else
  if Array.unsafe_get pool.available 7 = b then 7 else
  if Array.unsafe_get pool.available 8 = b then 8 else
  if Array.unsafe_get pool.available 9 = b then 9 else
  if Array.unsafe_get pool.available 10 = b then 10 else
  if Array.unsafe_get pool.available 11 = b then 11 else
  if Array.unsafe_get pool.available 12 = b then 12 else
  if Array.unsafe_get pool.available 13 = b then 13 else
  if Array.unsafe_get pool.available 14 = b then 14 else
  if Array.unsafe_get pool.available 15 = b then 15 else
  if Array.unsafe_get pool.available 16 = b then 16 else
  if Array.unsafe_get pool.available 17 = b then 17 else
  if Array.unsafe_get pool.available 18 = b then 18 else
  if Array.unsafe_get pool.available 19 = b then 19 else
    raise Assertion_failure (* Can't [assert false] because it would allocate *)

(* [@poll error] makes this function atomic from the point of view
   of threads within a single domain.
   This function either terminates the current systhread,
   or adds/readds it to the pool of available systhreads. *)
let[@poll error] keep_thread_or_exit pool (mbox : Mailbox.t) =
  if pool.terminating || pool.n_available = max_standby_systhreads_per_domain
  then raise Thread.Exit
  else (
    let i = find_thread_predicate_exists pool false in
    mbox.i <- i;
    pool.n_available <- pool.n_available + 1;
    Array.unsafe_set pool.available i true;
    Array.unsafe_set pool.threads i mbox
  )

(* [@poll error] makes this function atomic from the point of view
   of threads within a single domain.
   This function tries to reserve one available thread from the pool.
   Since we cannot return an option in [@poll error] code, we simulate
   an option by conditionally updating the reference [res].
   The returned boolean indicates whether we updated [res] or not. *)
let[@poll error] try_get_thread (pool : Mailbox.t t) res =
  if pool.n_available = 0
  then false
  else (
    let i = find_thread_predicate_exists pool true in
    pool.n_available <- pool.n_available - 1;
    Array.unsafe_set pool.available i false;
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
