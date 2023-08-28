open Eio.Std

let id = Fiber.create_key ()

let with_domain_tracing fn =
  Eio.Debug.with_trace_prefix (fun f ->
      Fiber.get id |> Option.iter (fun id -> Fmt.pf f "[%s] " id)
    ) fn

module Fake_domain_mgr = struct
  type t = {
    mutable next_domain_id : int;
  }

  let create () = { next_domain_id = 1 }

  let run t fn =
    let self = t.next_domain_id in
    t.next_domain_id <- t.next_domain_id + 1;
    let cancelled, _ = Promise.create () in
    Fiber.with_binding id (string_of_int self)
      (fun () -> fn ~cancelled)

  let run_raw t fn =
    let self = t.next_domain_id in
    t.next_domain_id <- t.next_domain_id + 1;
    Fiber.with_binding id (string_of_int self) fn
end

let create =
  let handler = Eio.Domain_manager.Pi.mgr (module Fake_domain_mgr) in
  fun () -> Eio.Resource.T (Fake_domain_mgr.create (), handler)

let run fn =
  let dm = create () in
  with_domain_tracing @@ fun () ->
  Fiber.with_binding id "0" @@ fun () ->
  fn dm
