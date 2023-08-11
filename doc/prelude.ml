#require "eio_main";;

module Eio_main = struct
  open Eio.Std

  let now = ref 1623940778.27033591

  module Fake_clock = struct
    type time = float
    type t = time Eio.Time.clock_ty r  (* The real clock *)

    let make real_clock = (real_clock :> t)

    let sleep_until real_clock time =
      (* The fake times are all in the past, so we just ask to wait until the
         fake time is due and it will happen immediately. If we wait for
         multiple times, they'll get woken in the right order. At the moment,
         the scheduler only checks for expired timers when the run-queue is
         empty, so this is a convenient way to wait for the system to be idle.
         TODO: This is no longer true (since #213). *)
      Eio.Time.sleep_until real_clock time;
      now := max !now time

    let now _ = !now
  end

  let fake_clock =
    let handler = Eio.Time.Pi.clock (module Fake_clock) in
    fun real_clock -> Eio.Resource.T (Fake_clock.make real_clock, handler)

  (* To avoid non-deterministic output, we run the examples a single domain. *)
  let fake_domain_mgr = object (_ : #Eio.Domain_manager.t)
    method run fn =
      (* Since we're in the same domain, cancelling the calling fiber will
         cancel the fake spawned one automatically. *)
      let cancelled, _ = Promise.create () in
      fn ~cancelled

    method run_raw fn = fn ()
  end

  let run fn =
    Eio_main.run @@ fun env ->
    fn @@ object
      method net         = env#net
      method stdin       = env#stdin
      method stdout      = env#stdout
      method stderr      = env#stderr
      method cwd         = env#cwd
      method process_mgr = env#process_mgr
      method domain_mgr  = fake_domain_mgr
      method clock       = fake_clock env#clock
    end
end

let parse_config (flow : _ Eio.Flow.source) = ignore
