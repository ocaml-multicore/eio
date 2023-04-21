(* To prevent races closing FDs, we do some ref-counting.

   Logically, the states of the wrapper for an FD [x] are:

   - open : the FD is available for use.
   - closing/users : no further operations can start, but some are still in progress.
   - closing/no-users : all operations have finished.
   - closing/closed : we no longer own the FD.

   We start by dividing ownership of [x] into [max_int] shares
   (enough shares for every sys-thread to take one).
   Having a fractional share (not 0 or 1) means you can use [x] but not close it.

   In the [open] and [closing/users] states, [t] owns the fraction
   [(max_int - ops) / max_int] of [x].

   In [closing/no-users], [t] owns all of [x].

   Initially, [t] is in the [open] state and [ops = 0].

   If you increment [ops] in [open] or [closing/users], you get one share and can use the FD
   (though in [closing/users] you should immediately return it).
   When you decrement [ops] in these cases, you give back your share and must not use the FD further.

   If you increment/decrement when in the state [closing/no-users] or [closing/closed],
   you do not get/return a share.

   A fiber can call {!close} to move from [open] at any time.
   If there are operations in progress when it does this, it transitions to [closing/users].
   Otherwise, it goes directly to [closing/no-users].
   The fiber that does this is known as the "closing fiber"
   and is responsible for finishing the close.

   We move from [closing/users] to [closing/no-users] when [ops] becomes 0
   (and [t] therefore owns all shares of [x]).
   [ops] may continue to change after this, but we never return to [closing/users]
   or give any user a share of [x] after this.

   From [closing/no-users], we transition to [closing/closed],
   transferring the now-full ownership of [x] to the closing fiber.

   In reality, the three [closing/*] states are represented by the single constructor [Closing],
   and the code must work whatever the true state might be. *)

type state =
  | Open of Unix.file_descr
  | Closing of (unit -> unit)     (* Function is called when [ops] becomes 0. *)

type t = {
  ops : int Atomic.t;
  fd : state Atomic.t;
}

let fully_closed = Closing ignore      (* Used for [closing/closed] *)

let put t =
  let old = Atomic.fetch_and_add t.ops (-1) in
  if old = 1 then (
    (* We decremented [ops] from one to zero. We may need to notify the closer. *)
    match Atomic.get t.fd with
    | Open _ -> ()     (* The fast path. We're not closing. *)
    | Closing no_users as prev ->
      (* There are four possibilities for the state when we did the decrement:
         - open: But it got closed after that. There could be new active users by now.
         - closing/users: We were the last user and transitioned to closing/no-users.
                          We need to notify the closer, or make sure someone else will do it later.
         - closing/no-users: We might need to notify, since a previous thread that reached zero
                             might have then seen [ops > 0] and deferred it to us.
         - closing/closed: No need to do anything, but notifying is harmless.
       *)
      if Atomic.get t.ops > 0 then ()  (* Someone else will deal with it. *)
      else if Atomic.compare_and_set t.fd prev fully_closed then (
        (* We observed [t.ops = 0] after closing, so we were then at either [closing/no-users]
           or [closing/closed], and we're now certainly at [closing/closed].
           If it was [closing/no-users] then we now own the FD, which we pass to the closer.
           If it was [closing/closed] then we don't, but [no_users] is [ignore] anyway. *)
        no_users ()
      ) else (
        (* Someone else notified the closer first. We're now in [closing/closed]. *)
      )
  ) else (
    assert (old > 1)
  )

let get t =
  Atomic.incr t.ops;
  (* If the state was [open] or [closing/users] then we now own 1 share of the FD. *)
  match Atomic.get t.fd with
  | Open fd ->
    (* The state was [open]. Give the share that we took to our caller. *)
    Some fd
  | Closing _ ->
    (* We want to close [t], so don't start a new operation.
       If the state was [open] or [closing/users] when we incremented [ops] then
       we return the share we took to [t] (which cannot now be [closing/no-users] as we are a user).
       Otherwise, it was [closing/no-users] or [closing/closed], and still is one of those. *)
    put t;
    None

(* Note: we could simplify this a bit by incrementing [t.ops], as [remove] does.
   However, that makes dscheck too slow. *)
let close t =
  match Atomic.get t.fd with
  | Closing _ ->
    (* Another caller closed [t] before us. *)
    false
  | Open fd as prev ->
    let next = Closing (fun () -> Unix.close fd) in
    if Atomic.compare_and_set t.fd prev next then (
      (* We just transitioned from [open] to [closing/users] or [closing/no-users].
         We are now the closer. *)
      if Atomic.get t.ops = 0 && Atomic.compare_and_set t.fd next fully_closed then (
        (* We were in [closing/no-users] and are now in [closing/closed].
           We own the FD (and our original callback will never be called). *)
        Unix.close fd
      ) else (
        (* The [next] callback remained installed and there is nothing left for us to do:
           - If [t.ops] was non-zero, another thread will eventually return it to zero and call our callback.
           - If the CAS failed, then another thread is invoking our callback. *)
      );
      true
    ) else (
      (* Another domain became the closer first. *)
      false
    )

let remove t =
  Atomic.incr t.ops;
  match Atomic.get t.fd with
  | Closing _ ->
    (* Another domain is dealing with it. *)
    put t;
    None
  | Open fd as prev ->
    Eio.Private.Suspend.enter_unchecked (fun _ctx enqueue ->
        if Atomic.compare_and_set t.fd prev (Closing (fun () -> enqueue (Ok (Some fd)))) then (
          (* We transitioned from [open] to [closing/users]. We are the closer. *)
          put t
        ) else (
          (* Another domain is handling the close instead. *)
          put t;
          enqueue (Ok (None))
        )
      )

let make fd =
  {
    ops = Atomic.make 0;
    fd = Atomic.make (Open fd);
  }

let is_open t =
  match Atomic.get t.fd with
  | Open _ -> true
  | Closing _ -> false

let use ~if_closed t f =
  match get t with
  | None -> if_closed ()
  | Some fd ->
    match f fd with
    | r -> put t; r
    | exception ex ->
      let bt = Printexc.get_raw_backtrace () in
      put t;
      Printexc.raise_with_backtrace ex bt

let peek t =
  match Atomic.get t.fd with
  | Open fd -> fd
  | Closing _ -> failwith "FD already closed!"

let pp f t =
  match Atomic.get t.fd with
  | Closing _ -> Fmt.string f "(closed FD)"
  | Open fd ->
    match Sys.os_type with
    | "Unix" ->
      let id : int = Obj.magic (fd : Unix.file_descr) in
      Fmt.pf f "FD-%d" id
    | _ ->
      Fmt.string f "(FD)"
