module Zzz = Eio_utils.Zzz

type job =
  | New
  | Exit
  | Job : {
      fn : unit -> 'a;
      enqueue : ('a, Eio.Exn.with_bt) result -> unit;
    } -> job

(* Mailbox with blocking semaphore *)
module Mailbox = struct
  type t = {
    available : Semaphore.Binary.t;
    mutable cell : job;
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

module Free_pool = struct
  type list =
    | Empty
    | Closed
    | Free of Mailbox.t * list

  type t = list Atomic.t

  let rec close_list = function
    | Free (x, xs) -> Mailbox.put x Exit; close_list xs
    | Empty | Closed -> ()

  let close t =
    let items = Atomic.exchange t Closed in
    close_list items

  let rec drop t =
    match Atomic.get t with
    | Closed | Empty -> ()
    | Free _ as items ->
      if Atomic.compare_and_set t items Empty then close_list items
      else drop t

  let rec put t mbox =
    match Atomic.get t with
    | Closed -> assert false
    | (Empty | Free _) as current ->
      let next = Free (mbox, current) in
      if not (Atomic.compare_and_set t current next) then
        put t mbox (* concurrent update, try again *)

  let make_thread t =
    let mbox = Mailbox.create () in
    let _thread : Thread.t = Thread.create (fun () ->
        while true do
          match Mailbox.take mbox with
          | New -> assert false
          | Exit -> raise Thread.Exit
          | Job { fn; enqueue } ->
            let result =
              try Ok (fn ())
              with exn ->
                let bt = Printexc.get_raw_backtrace () in
                Error (exn, bt)
            in
            put t mbox;         (* Ensure thread is in free-pool before enqueuing. *)
            enqueue result
        done
      ) ()
    in
    mbox

  let rec get_thread t =
    match Atomic.get t with
    | Closed -> invalid_arg "Thread pool closed!"
    | Empty -> make_thread t
    | Free (mbox, next) as current ->
      if Atomic.compare_and_set t current next then mbox
      else get_thread t (* concurrent update, try again *)
end

type t = {
  free : Free_pool.t;
  sleep_q : Zzz.t;
  mutable timeout : Zzz.Key.t option;
}

type _ Effect.t += Run_in_systhread : (unit -> 'a) -> (('a, Eio.Exn.with_bt) result * t) Effect.t

let terminate t =
  Free_pool.close t.free;
  Option.iter (fun key -> Zzz.remove t.sleep_q key; t.timeout <- None) t.timeout

let create ~sleep_q =
  { free = Atomic.make Free_pool.Empty; sleep_q; timeout = None }

let run t fn =
  match fn () with
  | x -> terminate t; x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    terminate t;
    Printexc.raise_with_backtrace ex bt

let submit t ~ctx ~enqueue fn =
  match Eio.Private.Fiber_context.get_error ctx with
  | Some e -> enqueue (Error (e, Eio.Exn.empty_backtrace))
  | None ->
    let mbox = Free_pool.get_thread t.free in
    Mailbox.put mbox (Job { fn; enqueue })

let run_in_systhread ?(label="systhread") fn =
  Eio.Private.Trace.suspend_fiber label;
  let r, t = Effect.perform (Run_in_systhread fn) in
  if t.timeout = None then (
    let time =
      Mtime.add_span (Mtime_clock.now ()) Mtime.Span.(20 * ms)
      |> Option.value ~default:Mtime.max_stamp
    in
    t.timeout <- Some (Zzz.add t.sleep_q time (Fn (fun () -> Free_pool.drop t.free; t.timeout <- None)))
  );
  match r with
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt
