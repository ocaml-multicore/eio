type t = Semaphore.t

let create () = Semaphore.make 1

let is_empty t = Semaphore.get_value t == 1

let is_locked t = Semaphore.get_value t == 0

let unlock = Semaphore.release

let lock = Semaphore.acquire

let with_lock t fn =
  lock t;
  Fun.protect ~finally:(fun () -> unlock t) fn
