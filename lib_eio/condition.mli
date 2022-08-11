(** Waiters call {!await} in a loop as long as some condition is false.
    Fibers that modify inputs to the condition must call [broadcast] soon
    afterwards so that waiters can re-check the condition.

    Example:

    {[
      let x = ref 0
      let cond = Eio.Condition.create ()
      let mutex = Eio.Mutex.create ()

      let set_x value =
        Eio.Mutex.use_rw ~protect:false mutex (fun () -> x := value);
        Eio.Condition.broadcast cond

      let await_x p =
        Eio.Mutex.use_ro mutex (fun () ->
           while not (p !x) do                  (* [x] cannot change, as mutex is locked. *)
             Eio.Condition.await ~mutex cond    (* Mutex is unlocked while suspended. *)
           done
        )
    ]}

    It is used like this:

    {[
      Fiber.both
        (fun () ->
           traceln "x = %d" !x;
           await_x ((=) 42);
           traceln "x = %d" !x
        )
        (fun () ->
           set_x 5;
           Fiber.yield ();
           set_x 7;
           set_x 42;
        )
    ]}
*)

type t

val create : unit -> t
(** [create ()] creates a new condition variable. *)

val await : t -> Eio_mutex.t -> unit
(** [await t mutex] suspends the current fiber until it is notified by [t].

    You should lock [mutex] before testing whether the condition is true,
    and leave it locked while calling this function.
    It will be unlocked while the fiber is waiting and locked again before
    returning (it is also locked again if the wait is cancelled). *)

val await_no_mutex : t -> unit
(** [await_no_mutex t] suspends the current fiber until it is notified by [t].

    This is only safe to use in the case where [t] is only used within a single domain,
    and the test for the condition was done without switching fibers.
    i.e. you know the condition is still false, and no notification of a change can be sent
    until [await_no_mutex] has finished suspending the fiber. *)

val broadcast : t -> unit
(** [broadcast t] wakes up any waiting fibers (by appending them to the run-queue to resume later).

    If no fibers are waiting, nothing happens. *)
