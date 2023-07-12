type job = Pack : (unit -> 'a) * ('a, exn) Result.t Promise.u -> job

type action =
  | Process of job
  | Quit of {
      atomic: int Atomic.t;
      target: int;
      all_done: unit Promise.u;
    }

(* Worker: 1 domain/thread
   m jobs per worker, n domains per workpool *)

type t = {
  sw: Switch.t;
  (* The work queue *)
  stream: action Stream.t;
  (* Number of domains. Depending on settings, domains may run more than 1 job at a time. *)
  domain_count: int;
  (* True when [Workpool.terminate] has been called. *)
  is_terminating: bool Atomic.t;
  (* Resolved when the workpool begins terminating. *)
  terminating: action Promise.t * action Promise.u;
  (* Resolved when the workpool has terminated. *)
  terminated: unit Promise.t * unit Promise.u;
}

let reject (Pack (_, w)) = Promise.resolve_error w (Failure "Workpool.terminate called")

(* This function is the core of workpool.ml.
   Each worker recursively calls [loop ()] until the [terminating]
   promise is resolved. Workers pull one job at a time from the Stream. *)
let start_worker ~limit ~terminating stream =
  Switch.run @@ fun sw ->
  let capacity = Semaphore.make limit in
  let run_job job w =
    Fiber.fork ~sw (fun () ->
        Promise.resolve w
          (try Ok (job ()) with
           | exn -> Error exn);
        Semaphore.release capacity )
  in
  (* The main worker loop. *)
  let rec loop () =
    let actions = Fiber.n_any [ (fun () -> Promise.await terminating); (fun () -> Semaphore.acquire capacity; Stream.take stream) ] in
    match actions with
    | [ Process (Pack (job, w)) ] ->
      (* We start the job right away. This also gives a chance to other domains
         to start waiting on the Stream before the current thread blocks on [Stream.take] again. *)
      run_job job w;
      (loop [@tailcall]) ()
    | Quit { atomic; target; all_done } :: maybe_job ->
      List.iter
        (function
          | Process job -> reject job
          | _ -> assert false)
        maybe_job;
        (* If we're the last worker terminating, resolve the promise
           after waiting until the completion of all of this worker's jobs. *)
      if Atomic.fetch_and_add atomic 1 = target
      then Switch.on_release sw (Promise.resolve all_done)
    | _ -> assert false
  in
  loop ()

(* Start a new domain. The worker will need a switch, then we start the worker. *)
let start_domain ~sw ~domain_mgr ~limit ~terminating ~transient stream =
  let go () =
    Domain_manager.run domain_mgr (fun () -> start_worker ~limit ~terminating stream )
  in
  (* [transient] workpools run as daemons to not hold the user's switch from completing.
     It's up to the user to hold the switch open (and thus, the workpool)
     by blocking on the jobs issued to the workpool.
     [Workpool.submit] and [Workpool.submit_exn] will block so this shouldn't be a problem.
     Still, the user can call [Workpool.create] with [~transient:false] to
     disable this behavior, in which case the user must call [Workpool.terminate]
     to release the switch. *)
  match transient with
  | false -> Fiber.fork ~sw go
  | true ->
    Fiber.fork_daemon ~sw (fun () ->
        go ();
        `Stop_daemon )

let create ~sw ~domain_count ~domain_concurrency ?(transient = true) domain_mgr =
  let stream = Stream.create 0 in
  let instance =
    {
      sw;
      stream;
      domain_count;
      is_terminating = Atomic.make false;
      terminating = Promise.create ();
      terminated = Promise.create ();
    }
  in
  let terminating = fst instance.terminating in
  for _ = 1 to domain_count do
    start_domain ~sw ~domain_mgr ~limit:domain_concurrency ~terminating ~transient stream
  done;
  instance

let submit_fork ~sw { stream; _ } f =
  let p, w = Promise.create () in
  Fiber.fork_promise ~sw (fun () ->
      Stream.add stream (Process (Pack (f, w)));
      Promise.await_exn p )

let submit { stream; _ } f =
  let p, w = Promise.create () in
  Stream.add stream (Process (Pack (f, w)));
  Promise.await p

let submit_exn instance f =
  match submit instance f with
  | Ok x -> x
  | Error exn -> raise exn

let terminate ({ terminating = _, w1; terminated = p2, w2; _ } as instance) =
  if Atomic.compare_and_set instance.is_terminating false true
  then (
    (* Instruct workers to shutdown *)
    Promise.resolve w1 (Quit { atomic = Atomic.make 1; target = instance.domain_count; all_done = w2 });
    (* Reject all present and future queued jobs *)
    Fiber.fork_daemon ~sw:instance.sw (fun () ->
        while true do
          match Stream.take instance.stream with
          | Process job -> reject job
          | _ -> assert false
        done;
        `Stop_daemon );
    (* Wait for all workers to have shutdown *)
    Promise.await p2 )
  else (* [Workpool.terminate] was called more than once. *)
    Promise.await p2

let is_terminating { is_terminating; _ } = Atomic.get is_terminating

let is_terminated { terminated = p, _; _ } = Promise.is_resolved p
