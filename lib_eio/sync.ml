(* A lock-free synchronous channel with cancellation, using Cells.

   Producers and consumers are paired off and then the producer transfers its
   value to the consumer. This is effectively a bounded queue with a capacity
   of zero.

   Both producers and consumers can cancel while waiting.

   There is an atomic int ([balance]), plus two queues ([consumers] and
   [producers]) made using Cells. When [balance] is positive, it is the number
   of producers waiting with values that no one is yet responsible for
   resuming. When negative, it is the (negative) number of waiting consumers
   that no one is responsible for resuming.

   To put an item:

   1. The producer increments [balance].
   2. If it was negative, the producer resumes one waiting consumer on the [consumers] queue.
      Otherwise, it suspends itself on the [producers] queue.

   To take an item:

   1. The consumer decrements [balance].
   2. If it was positive, the consumer resumes one waiting producer on the [producers] queue.
      Otherwise, it suspends itself on the [consumers] queue.

   Therefore, we never try to resume on a queue unless another party has
   started the process of suspending on it.

   The system will not become idle while a client is responsible for resuming
   something. Therefore, when idle:

   - If [balance <= 0] then there are no waiting producers.
   - If [balance >= 0] then there are no waiting consumers.
   - So, we never have waiting consumers and producers at the same time.

   As usual with Cells, either party may get to the new cell first. Whichever party
   arrives first writes a callback, which the other party will then call when they arrive.

   Note on terminology:

   - The "suspender" of a cell is the party that incremented the queue's suspend index,
     and the "resumer" of a cell is the party that incremented the resume index.

   - Whether "suspending" or "resuming" a cell, you may still have to suspend
     your fiber and resume it later.

   States

   There are four cell states:

   - [In_transition] indicates that the cell is still being initialised, or might be
     getting cancelled. Either way, the suspending party is actively working to
     change the cell's state.

   - [Item] indicates that the producer is ready to provide an item.

   - [Slot] indicates that the consumer is ready to receive an item.

   - [Finished] indicates that the cell is no longer being used (the value has
     been consumed or the cell has finished being cancelled).

   The possible sequences of states on the [producers] queue are:

   In_transition -C> Slot -P> Finished    (consumer arrives first)
                 `P> Item -C> Finished    (producer arrives first)
                          `P> In_transition -P> Finished   (producer cancels)
                                            `C> Slot -P> Finished   (cancellation interrupted)

   Only the producer can cancel here. For the [consumers] queue it's the
   opposite - the consumer can cancel its [Slot].

   Cancellation

   Note that there are two kinds of cancellation here:

   1. A cancelled cell is not considered part of its queue. Anyone seeing one
      (due to a race) will skip over it and use the next cell.

   2. After a consumer and producer have been paired off (and the cell removed
      from its queue), the consumer callback may reject the value. If this
      happens, the producer must start all over again to find another consumer.

   Whenever a consumer sets its callback to reject values, it should then start
   the process of cancelling its cell (if acting as a suspender) so that the
   cell can be GC'd.

   A consumer can only cancel its cell when it's on the [consumers] queue.
   If it's on [producers], it knows a wake up will be coming shortly anyway.
   A consumer cancels its cell as follows:

   1. The consumer sets its cell in [consumers] to [In_transition].
   2. It increments [balance] (from a negative value). It is now committed to cancelling.
   3. It sets its cell to [Finished].

   (1) will fail if the cell got resumed first. In that case the consumer just
   rejects the cancellation attempt.

   (2) will fail if [balance >= 0]. In that case the consumer has not cancelled,
   and is about to be resumed instead. It tries to return to the [Slot] state.
   If that fails, the cell now contains an Item and the consumer takes it.

   (3) will fail if a producer arrived after the consumer committed to cancelling.
   In that case, the consumer passes the Item on to the next consumer (there
   must be another one, since both the consumer and producer incremented
   [balance] from a negative value).

   Cancelling a producer is very similar to cancelling a consumer, just with the
   [producers] queue and decrementing the balance from a positive value.

   Non-blocking take

   To perform a non-blocking take:

   1. The consumer decrements [balance] from a positive number.
   2. The consumer takes the next resume cell from [producers].
   3. The consumer takes the [Item] from the cell, setting it to [Finished].

   (1) will fail if there are no unassigned items available.
   Then the [take_nonblocking] returns [None], as there are no items waiting.

   (3) will fail if the producer is initialising or cancelling. In either case,
   the consumer sets its cell to a request with a dummy callback that rejects
   all values and continues immediately.

   The exchange

   Once a producer and consumer have been paired off (and so their cell is now Finished),
   the producer's value is passed to the consumer's callback. If the consumer accepts it,
   then both fibers are resumed. If not, the producer starts again (incrementing [balance]
   again) and waits for another consumer.

   The above has not been formally verified (exercise for reader!). *)

(* Import these directly because we copy this file for the dscheck tests. *)
module Fiber_context = Eio__core.Private.Fiber_context
module Suspend = Eio__core.Private.Suspend
module Cancel = Eio__core.Cancel

type 'a item = {
  v : 'a;
  kp : (bool, exn) result -> unit;      (* [Ok false] means consumer refused the item; retry. *)
  cancel : [
    | `Resuming                         (* In the process of resuming, so can't cancel. *)
    | `Suspended of (unit -> bool)      (* Call this function to attempt to leave the queue. *)
    | `Cancelled of exn                 (* Already cancelled. *)
  ] Atomic.t;
}

type 'a cell =
  | In_transition
  | Slot of ('a -> bool)
  | Item of 'a item
  | Finished

module Cell = struct
  type 'a t = 'a cell

  let init = In_transition

  let segment_order = 2

  let dump f = function
    | In_transition -> Fmt.string f "In_transition"
    | Slot _ -> Fmt.string f "Slot"
    | Item _ -> Fmt.string f "Item"
    | Finished -> Fmt.string f "Finished"
end

module Q = Cells.Make(Cell)

type 'a t = {
  balance : int Atomic.t;
  consumers : 'a Q.t;
  producers : 'a Q.t;
}

type 'a loc =
  | Short of 'a Cell.t Atomic.t                 (* Acting as resumer of cell *)
  | Long of ('a Q.segment * 'a Cell.t Atomic.t) (* Acting as suspender of cell; can cancel *)

let dump f t =
  Fmt.pf f "@[<v2>Sync (balance=%d)@,@[<v2>Consumers:@,%a@]@,@[<v2>Producers:@,%a@]@]"
    (Atomic.get t.balance)
    Q.dump t.consumers
    Q.dump t.producers

(* Give [item] to consumer [kc]. [item]'s cell is now Finished. *)
let exchange item kc = item.kp (Ok (kc item.v))

(* Add [value] to [cell].
   If the cell is in transition, place [value] there and let the other party handle it later.
   If the peer's value is already present, do the exchange.
   If the peer cancelled the cell then try the next one on the given resume queue (if we're adding
   to a suspend queue then it can't be cancelled, because the caller controls cancellation).
   This is only used when our fiber is already suspended,
   since we can't create [value] before we have the continuation. *)
let rec add_to_cell queue value cell =
  match Atomic.get cell, value with
  | Finished, _ -> add_to_cell queue value (Q.next_resume queue)     (* Cancelled - skip *)
  | (Slot kc   as old), Item item
  | (Item item as old), Slot kc ->
    if Atomic.compare_and_set cell old Finished then exchange item kc
    else add_to_cell queue value cell
  | In_transition, _ ->
    if Atomic.compare_and_set cell In_transition value then ()
    else add_to_cell queue value cell
  | (Slot _ | Item _), _ -> assert false

(* Cancelling *)

let rec decr_balance_if_positive t =
  let cur = Atomic.get t.balance in
  if cur > 0 then (
    if Atomic.compare_and_set t.balance cur (cur - 1) then true
    else decr_balance_if_positive t
  ) else false

let rec incr_balance_if_negative t =
  let cur = Atomic.get t.balance in
  if cur < 0 then (
    if Atomic.compare_and_set t.balance cur (cur + 1) then true
    else incr_balance_if_negative t
  ) else false

(* Cancel [cell] on our suspend queue.
   This function works for both consumers and producers, as we can tell from
   the value what our role is (and if there isn't a value, we're finished anyway).
   Neither party will try to cancel before writing its own value.
   Returns [true] if the caller cancelled successfully,
   or [false] if it must wait (as it's being resumed). *)
let cancel t (segment, cell) =
  let cancel2 update_balance ~old =
    if Atomic.compare_and_set cell old In_transition then (
      if update_balance t then (
        (* At this point, we are committed to cancelling. *)
        begin match Atomic.exchange cell Finished with
          | Finished -> assert false
          | In_transition -> Q.cancel_cell segment
          | Item request -> add_to_cell t.consumers (Item request) (Q.next_resume t.consumers)
          | Slot kc      -> add_to_cell t.producers (Slot kc)      (Q.next_resume t.producers)
        end;
        true
      ) else (
        (* We decided not to cancel. We know a resume is coming. *)
        if Atomic.compare_and_set cell In_transition old then false
        else (
          match old, Atomic.get cell with
          | Slot kc, Item request
          | Item request, Slot kc ->
            Atomic.set cell Finished;
            exchange request kc;
            false
          | _ -> assert false
        )
      )
    ) else false          (* The peer resumed us first *)
  in
  match Atomic.get cell with
  | Finished -> false     (* The peer resumed us first *)
  | Slot _ as old -> cancel2 incr_balance_if_negative ~old      (* We are a consumer *)
  | Item _ as old -> cancel2 decr_balance_if_positive ~old      (* We are a producer *)
  | In_transition ->
    (* Either we're initialising the cell, in which case we haven't told the
       application how to cancel this location yet, or we're already
       cancelling, but cancelling twice isn't permitted. *)
    assert false

(* A producer can't cancel if it is resuming on the [consumers] queue, and will instead
   just wait for the slot in that case, which will arrive soon. However, after getting
   a slot the producer may be rejected and be asked to start again on the [producers] queue,
   so we need to remember that we were cancelled to prevent that. It's also possible that
   we're already restarting but haven't got around to updating [request.cancel] yet; we'll
   notice the new [`Cancelled] state when we do. *)
let cancel_put request ex =
  match Atomic.exchange request.cancel (`Cancelled ex) with
  | `Cancelled _ -> failwith "Already cancelled!"
  | `Resuming -> false  (* Cancellation fails for now, but we remember we wanted to cancel. *)
  | `Suspended cancel -> cancel ()

(* Putting. *)

(* Like [add_to_cell], but we haven't created our value yet as we haven't suspended the fiber. *)
let rec producer_resume_cell t ~success ~in_transition cell =
  match Atomic.get (cell : _ Cell.t Atomic.t) with
  | Item _ -> assert false
  | In_transition -> in_transition cell
  | Finished -> producer_resume_cell t ~success ~in_transition (Q.next_resume t.consumers)
  | Slot k as old ->
    if Atomic.compare_and_set cell old Finished then success k
    else producer_resume_cell t ~success ~in_transition cell

(* This is essentially the main [put] function, but parameterised so it can be shared with
   the rejoin-after-rejection case. *)
let producer_join (t : _ t) ~success ~suspend =
  let old = Atomic.fetch_and_add t.balance (+1) in
  if old < 0 then (
    let cell = Q.next_resume t.consumers in
    producer_resume_cell t cell
      ~success
      ~in_transition:(fun cell -> suspend (Short cell))
  ) else (
    suspend (Long (Q.next_suspend t.producers))
  )

(* Called when a consumer took our value but then rejected it.
   We start the put operation again, except that our fiber is already suspended
   so no need to do that again. We're probably running in the consumer's domain
   (unless the consumer provided their callback while we were cancelling). *)
let put_already_suspended t request =
  producer_join t
    ~success:(exchange request)
    ~suspend:(fun loc ->
        let Short cell | Long (_, cell) = loc in
        add_to_cell t.consumers (Item request) cell;
        let rec aux () =
          match Atomic.get request.cancel, loc with
          | (`Suspended _ | `Resuming as prev), Long loc ->
            (* We might be suspended for a while. Update the cancel function with the new location. *)
            let cancel_fn () = cancel t loc in
            if not (Atomic.compare_and_set request.cancel prev (`Suspended cancel_fn)) then aux ()
          | `Cancelled ex, Long loc ->
            (* We got cancelled after the peer removed our cell and before we updated the
               cancel function with the new location, or we were cancelled while doing a
               (non-cancellable) resume. Deal with it now. *)
            if cancel t loc then request.kp (Error ex);
            (* else we got resumed first *)
          | _, Short _ ->
            (* We can't cancel while in the process of resuming a cell on the [consumers] queue.
               We could set [cancel] to [`Resuming] here, but there's no need as trying to use the
               old cancel function will find the old cell is cancelled and set [request.cancel]
               to [`Cancelled]), as required. *)
            ()
        in aux ()
      )

(* We tried to [put] and no slot was immediately available.
   Suspend the fiber and use the continuation to finish initialising the cell.
   Note that we may be suspending the fiber even when using the "resume" queue,
   if the consumer is still in the process of writing its slot. *)
let put_suspend t v loc =
  Suspend.enter_unchecked @@ fun ctx enqueue ->
  let cancel =
    match loc with
    | Short _ -> `Resuming      (* Can't cancel this *)
    | Long loc -> `Suspended (fun () -> cancel t loc)
  in
  let rec item = {
    v;
    cancel = Atomic.make cancel;
    kp = function
      | Error _ as e -> enqueue e                 (* Cancelled by [put_already_suspended]. *)
      | Ok true -> enqueue (Ok ())                (* Success! *)
      | Ok false -> put_already_suspended t item  (* Consumer rejected value. Restart. *)
  } in
  let Short cell | Long (_, cell) = loc in
  add_to_cell t.consumers (Item item) cell;
  (* Set up the cancel handler in either case because we might change queues later: *)
  match Fiber_context.get_error ctx with
  | Some ex ->
    if cancel_put item ex then enqueue (Error ex);
    (* else being resumed *)
  | None ->
    Fiber_context.set_cancel_fn ctx (fun ex ->
        if cancel_put item ex then enqueue (Error ex)
        (* else being resumed *)
      )

let rec put (t : _ t) v =
  producer_join t
    ~success:(fun kc -> if kc v then () else put t v)
    ~suspend:(put_suspend t v)

(* Taking. *)

(* Mirror of [producer_resume_cell]. *)
let rec consumer_resume_cell t ~success ~in_transition cell =
  match Atomic.get (cell : _ Cell.t Atomic.t) with
  | Slot _ -> assert false
  | In_transition -> in_transition cell
  | Finished -> consumer_resume_cell t ~success ~in_transition (Q.next_resume t.producers)
  | Item req as old ->
    if Atomic.compare_and_set cell old Finished then success req
    else consumer_resume_cell t ~success ~in_transition cell

let take_suspend t loc =
  Suspend.enter_unchecked @@ fun ctx enqueue ->
  let Short cell | Long (_, cell) = loc in
  let kc v = enqueue (Ok v); true in
  add_to_cell t.producers (Slot kc) cell;
  match loc with
  | Short _ -> ()
  | Long loc ->
    match Fiber_context.get_error ctx with
    | Some ex ->
      if cancel t loc then enqueue (Error ex);
      (* else being resumed *)
    | None ->
      Fiber_context.set_cancel_fn ctx (fun ex ->
          if cancel t loc then enqueue (Error ex)
          (* else being resumed *)
        )

let take (t : _ t) =
  let old = Atomic.fetch_and_add t.balance (-1) in
  if old > 0 then (
    let cell = Q.next_resume t.producers in
    consumer_resume_cell t cell
      ~success:(fun item -> item.kp (Ok true); item.v)
      ~in_transition:(fun cell -> take_suspend t (Short cell))
  ) else (
    take_suspend t (Long (Q.next_suspend t.consumers))
  )

let reject = Slot (fun _ -> false)

let take_nonblocking (t : _ t) =
  if decr_balance_if_positive t then (
    let rec aux cell =
      consumer_resume_cell t cell
        ~success:(fun item ->
            item.kp (Ok true);          (* Always accept the item *)
            Some item.v
          )
        ~in_transition:(fun cell ->
            (* Our producer is still in the process of writing its [Item], but
               we're non-blocking and can't wait. We're always acting as the
               resumer, so we can't cancel the cell. Instead, we provide a
               consumer callback that always rejects.
               todo: could spin for a bit here first - the Item will probably arrive soon,
               and that would avoid making the producer start again. *)
            Domain.cpu_relax ();        (* Brief wait to encourage producer to finish *)
            if Atomic.compare_and_set cell In_transition reject then None
            else aux cell
          )
    in aux (Q.next_resume t.producers)
  ) else None   (* No waiting producers for us *)

(* Creation and status. *)

let create () =
  {
    consumers = Q.make ();
    producers = Q.make ();
    balance = Atomic.make 0;
  }

let balance t = Atomic.get t.balance
