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
         Will need revising if we make the scheduler fair at some point. *)
      Eio.Time.sleep_until real_clock time;
      now := max !now time
  end

  (* To avoid non-deterministic output, we run the examples a single domain. *)
  let fake_domain_mgr = object (_ : #Eio.Domain_manager.t)
    method run fn = fn ()
    method run_raw fn = fn ()
  end

  (* https://github.com/ocaml/ocaml/issues/10324 *)
  let dontcrash = Sys.opaque_identity

  let run fn =
    Eio_main.run @@ fun env ->
    fn @@ object
      method net        = dontcrash env#net
      method stdin      = dontcrash env#stdin
      method stdout     = dontcrash env#stdout
      method cwd        = dontcrash env#cwd
      method domain_mgr = fake_domain_mgr
      method clock      = fake_clock env#clock
    end
end
