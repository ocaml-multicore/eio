#require "eio_main";;

module Eio_main = struct
  open Eio.Std

  let now = ref 1623940778.27033591

  let fake_clock real_clock = object (_ : #Eio.Time.clock)
    method now = !now
    method sleep_until time =
      (* The fake times are all in the past, so we just ask to wait until the
         fake time is due and it will happen immediately. If we wait for
         multiple times, they'll get woken in the right order. At the moment,
         the scheduler only checks for expired timers when the run-queue is
         empty, so this is a convenient way to wait for the system to be idle.
         TODO: This is no longer true (since #213). *)
      Eio.Time.sleep_until real_clock time;
      now := max !now time
  end

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
      method net        = env#net
      method stdin      = env#stdin
      method stdout     = env#stdout
      method cwd        = env#cwd
      method domain_mgr = fake_domain_mgr
      method clock      = fake_clock env#clock
    end
end

let parse_config (flow : #Eio.Flow.source) = ignore
