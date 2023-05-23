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
  max_size : int;
  mutable current_size : int;
  alloc : 'a alloc;
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
}

let create
    ?(init_size = 0)
    ~(alloc : 'a alloc)
    max_size
  : 'a t =
  if init_size < 0 then (
    invalid_arg "Pool.create: init_size is negative"
  );
  if max_size < init_size then (
    invalid_arg "Pool.create: max_size is smaller than init_size"
  );
  let ready = Stream.create max_size in
  for _ = 0 to init_size-1 do
    Stream.add ready (alloc ())
  done;
  {
    lock = Eio_mutex.create ();
    max_size;
    current_size = init_size;
    alloc;
    ready;
    clear_signal = Atomic.make false;
  }

let async
    ~sw
    (t : 'a t)
    (f : 'a -> unit)
  : unit =
  let elem =
    Eio_mutex.use_rw ~protect:true t.lock (fun () ->
        match Stream.take_nonblocking t.ready with
        | None -> (
            if t.current_size < t.max_size then (
              t.current_size <- t.current_size + 1;
              Some (t.alloc ())
            ) else (
              None
            )
          )
        | Some x -> Some x
      )
  in
  let elem, handlers =
    match elem with
    | None -> Stream.take t.ready
    | Some x -> x
  in
  (* Obtain a copy of clear signal for this runner *)
  let clear_signal = t.clear_signal in
  Fiber.fork ~sw (fun () ->
      Fun.protect
        (fun () -> f elem)
        ~finally:(fun () ->
            let do_not_clear = not (Atomic.get clear_signal) in
            Eio_mutex.use_rw ~protect:true t.lock (fun () ->
                if do_not_clear && handlers.check elem then (
                  Stream.add t.ready (elem, handlers)
                ) else (
                  assert (t.current_size > 0);
                  t.current_size <- t.current_size - 1;
                  handlers.dispose elem;
                )
              )
          )
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
      Atomic.set t.clear_signal true;
      t.clear_signal <- Atomic.make false;
    )
