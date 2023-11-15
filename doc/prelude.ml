#require "eio_main";;
#require "eio.mock";;

module Eio_main = struct
  open Eio.Std

  module Fake_clock = struct
    type time = float
    type t = unit
    let sleep_until () _time = failwith "No sleeping in tests!"
    let now _ = 1623940778.27033591
  end

  let fake_clock =
    let handler = Eio.Time.Pi.clock (module Fake_clock) in
    Eio.Resource.T ((), handler)

  let run fn =
    (* To avoid non-deterministic output, we run the examples a single domain. *)
    let fake_domain_mgr = Eio_mock.Domain_manager.create () in
    Eio_main.run @@ fun env ->
    fn @@ object
      method net         = env#net
      method stdin       = env#stdin
      method stdout      = env#stdout
      method stderr      = env#stderr
      method cwd         = env#cwd
      method process_mgr = env#process_mgr
      method domain_mgr  = fake_domain_mgr
      method clock       = fake_clock
    end
end

let parse_config (flow : _ Eio.Flow.source) = ignore
