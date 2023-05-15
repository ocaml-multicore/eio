type 'a alloc = unit -> 'a

type 'a check = 'a -> bool

type 'a dispose = 'a -> unit

type 'a t = {
  lock : Eio_mutex.t;
  max_size : int;
  mutable current_size : int;
  alloc : 'a alloc;
  check : 'a check;
  dispose : 'a dispose;
  ready : 'a Stream.t;
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
    ?(check : 'a check = fun _ -> true)
    ?(dispose : 'a dispose = fun _ -> ())
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
    check;
    dispose;
    ready;
    clear_signal = Atomic.make false;
  }

let async 
    (t : 'a t)
    (f : 'a -> 'b)
  : 'b Promise.t =
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
  let elem =
    match elem with
    | None -> Stream.take t.ready
    | Some x -> x
  in
  let promise, resolver = Promise.create () in
  (* Obtain a copy of clear signal for this runner *)
  let clear_signal = t.clear_signal in
  Switch.run (fun sw ->
      Fiber.fork ~sw (fun () ->
          Fun.protect
            (fun () ->
               let res = f elem in
               Promise.resolve resolver res
            )
            ~finally:(fun () ->
                let do_not_clear = not (Atomic.get clear_signal) in
                if do_not_clear && t.check elem then (
                  Stream.add t.ready elem
                ) else (
                  Eio_mutex.use_rw ~protect:true t.lock (fun () ->
                      assert (t.current_size > 0);
                      t.current_size <- t.current_size - 1;
                      t.dispose elem;
                    )
                )
              )
        )
    );
  promise

let use t f =
  Promise.await (async t f)

let clear (t : 'a t) =
  Eio_mutex.use_rw ~protect:true t.lock (fun () ->
      Atomic.set t.clear_signal true;
      t.clear_signal <- Atomic.make false;
    )
