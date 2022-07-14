#require "eio_main";;

module Eio_main = struct
  open Eio.Std

  let now_ns = ref 16571892057187722L

  let fake_clock real_clock = object (_ : #Eio.Time.clock)
    method now_ns = !now_ns
    method sleep_until time =
      (* The fake times are all in the past, so we just ask to wait until the
         fake time is due and it will happen immediately. If we wait for
         multiple times, they'll get woken in the right order. At the moment,
         the scheduler only checks for expired timers when the run-queue is
         empty, so this is a convenient way to wait for the system to be idle.
         TODO: This is no longer true (since #213). *)
      Eio.Time.sleep_until real_clock time;
      now_ns := Int64.max !now_ns time
  end

  (* To avoid non-deterministic output, we run the examples a single domain. *)
  let fake_domain_mgr = object (_ : #Eio.Domain_manager.t)
    method run fn = fn ()
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
      method sys_clock  = fake_clock env#sys_clock
      method mono_clock = fake_clock env#mono_clock
    end
end
