(* This thread pool does not spawn threads in advance,
   but up to [max_standby_systhreads_per_domain] threads are
   kept alive to wait for more work to arrive.
   This number was chosen somewhat arbitrarily but benchmarking
   shows it to be a good compromise. *)
let max_standby_systhreads_per_domain = 25

type job =
| New
| Exit
| Job : {
  fn: unit -> 'a;
  enqueue: ('a, exn) result -> unit;
} -> job

(* Mailbox with blocking semaphore *)
module Mailbox = struct
  type t = {
    available: Semaphore.Binary.t;
    mutable cell: job;
  }

  let create () = { available = Semaphore.Binary.make false; cell = New }

  let put mbox x =
    (* The Semaphore contains an atomic frontier,
       therefore [cell] does not need to be an atomic *)
    mbox.cell <- x;
    Semaphore.Binary.release mbox.available

  let take mbox =
    Semaphore.Binary.acquire mbox.available;
    mbox.cell
end

(* A lock-free Treiber stack of systhreads on stand-by.
   A fresh thread is created if no thread is immediately available.
   When the domain exits all thread on stand-by are shutdown. *)
type t = {
  threads: (Mailbox.t * int) list Atomic.t;
  terminating: bool Atomic.t;
}

let create () = { threads = Atomic.make []; terminating = Atomic.make false }

let terminate { threads; terminating } =
  Atomic.set terminating true;
  List.iter (fun (mbox, _) -> Mailbox.put mbox Exit) (Atomic.get threads)

let rec keep_thread_or_exit ({ threads; _ } as pool) mbox =
  match Atomic.get threads with
  | (_, count) :: _ when count >= max_standby_systhreads_per_domain ->
    (* We've got enough threads on stand-by, so discard the current thread *)
    raise Thread.Exit
  | current ->
    let count = match current with
    | [] -> 0
    | (_, count) :: _ -> count
    in
    if not (Atomic.compare_and_set threads current ((mbox, count + 1) :: current))
    then keep_thread_or_exit pool mbox (* concurrent update, try again *)

let make_thread pool =
  let mbox = Mailbox.create () in
  let _t : Thread.t = Thread.create (fun () ->
    while true do
      match Mailbox.take mbox with
      | New -> assert false
      | Exit -> raise Thread.Exit
      | Job { fn; enqueue } ->
        enqueue (try Ok (fn ()) with exn -> Error exn);
        (* We're not yielding inside of [keep_thread_or_exit] so
           no need to check [terminating] multiple times *)
        if Atomic.get pool.terminating then raise Thread.Exit;
        keep_thread_or_exit pool mbox
    done
  ) ()
  in
  mbox

let rec get_thread ({ threads; _ } as pool) =
  match Atomic.get threads with
  | [] -> make_thread pool
  | ((mbox, _count) :: rest) as current ->
    if not (Atomic.compare_and_set threads current rest)
    then get_thread pool (* concurrent update, try again *)
    else mbox

(* https://v2.ocaml.org/manual/parallelism.html#s:par_systhread_interaction
   "Only one systhread at a time is allowed to run OCaml code on a particular domain."
   So we keep a separate threadpool per domain. *)
let key =
  Domain.DLS.new_key @@ fun () ->
    let pool = create () in
    Domain.at_exit (fun () -> terminate pool);
    pool

let run_on_systhread ~enqueue fn =
  let pool = Domain.DLS.get key in
  let mbox = get_thread pool in
  Mailbox.put mbox (Job { fn; enqueue })
