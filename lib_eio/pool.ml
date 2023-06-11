type 'a check = 'a -> bool

type 'a dispose = 'a -> unit

type 'a handlers = {
  check : 'a check;
  dispose : 'a dispose;
}

let noop_handlers : 'a handlers = {
  check = (fun _ -> true);
  dispose = (fun _ -> ());
}

type 'a ready = 'a * 'a handlers

type 'a alloc = unit -> 'a ready

type 'a t = {
  lock : Eio_mutex.t;
  alloc_budget : Semaphore.t;
  alloc : 'a alloc;
  waiting : 'a ready Promise.u option Stream.t;
  ready : 'a ready Stream.t;
  (* Each runner is given a copy of the clear signal
     to be checked after its run

     The copy to use is replaced after each invocation
     of [clear], so we can distinguish between
     clearing of different batch of runs, e.g.

     1. use (clear signal version 0)
     2. use (clear signal version 0)
     3. clear
     4. use (clear signal version 1)

     clear at 3 should apply to use at 1 and 2, but not use at 4
  *)
  mutable clear_signal : bool Atomic.t;
  shutdown : bool Atomic.t;
}

let start_monitor ~sw (t : 'a t) : unit =
  let rec aux () =
    match Stream.take t.waiting with
    | None -> ()
    | Some resolver -> (
        Semaphore.acquire t.alloc_budget;
        Eio_mutex.use_rw ~protect:true t.lock (fun () ->
            match Stream.take_nonblocking t.ready with
            | None -> (
                match t.alloc () with
                | x -> Promise.resolve resolver x
                | exception exn -> Switch.fail sw exn
              )
            | Some x -> Promise.resolve resolver x
          );
        aux ()
      )
  in
  Fiber.fork ~sw aux

let create
    ~sw
    ~(alloc : 'a alloc)
    max_size
  : 'a t =
  if max_size <= 0 then (
    invalid_arg "Pool.create: max_size is <= 0"
  );
  let t =
    {
      lock = Eio_mutex.create ();
      alloc_budget = Semaphore.make max_size;
      alloc;
      waiting = Stream.create max_size;
      ready = Stream.create max_size;
      clear_signal = Atomic.make false;
      shutdown = Atomic.make false;
    }
  in
  start_monitor ~sw t;
  t

let async
    ~sw
    (t : 'a t)
    (f : 'a -> unit)
  : unit =
  if Atomic.get t.shutdown then (
    invalid_arg "Pool.async: Pool already shutdown"
  );
  let (promise, resolver) : 'a ready Promise.t * 'a ready Promise.u =
    Promise.create ()
  in
  Stream.add t.waiting (Some resolver);
  let elem, handlers = Promise.await promise in
  (* Obtain a copy of clear signal for this runner *)
  let clear_signal = t.clear_signal in
  Fiber.fork ~sw (fun () ->
      let r =
        match f elem with
        | () -> None
        | exception exn -> Some exn
      in
      let do_not_clear = not (Atomic.get clear_signal) in
      if do_not_clear && handlers.check elem then (
        Stream.add t.ready (elem, handlers)
      ) else (
        handlers.dispose elem
      );
      Semaphore.release t.alloc_budget;
      match r with
      | None -> ()
      | Some exn -> Switch.fail sw exn
    )

let async_promise
    ~sw
    (t : 'a t)
    (f : 'a -> 'b)
  : 'b Promise.or_exn =
  let promise, resolver = Promise.create () in
  async ~sw t (fun x ->
      match f x with
      | res -> Promise.resolve_ok resolver res
      | exception exn -> Promise.resolve_error resolver exn);
  promise

let use t f =
  Switch.run (fun sw ->
      match Promise.await (async_promise ~sw t f) with
      | Ok x -> x
      | Error exn -> raise exn
    )

let clear (t : 'a t) =
  Eio_mutex.use_rw ~protect:true t.lock (fun () ->
      let old_signal = t.clear_signal in
      Atomic.set old_signal true;
      t.clear_signal <- Atomic.make false;
    )

let shutdown (t : 'a t) =
  clear t;
  Atomic.set t.shutdown true;
  Stream.add t.waiting None
