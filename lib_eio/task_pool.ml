type 'a task = unit -> 'a

type runner = unit task -> unit

type t = {
  sw : Switch.t;
  runners : runner Pool.t;
}

let create ~sw ~max_domains domain_mgr : t =
  let alloc () : runner * runner Pool.handlers =
    let s : (unit task * unit Promise.u) option Stream.t =
      Stream.create 0
    in
    let is_okay = Atomic.make true in
    Fiber.fork ~sw (fun () ->
        Domain_manager.run domain_mgr (fun () ->
            let rec aux () =
              match Stream.take s with
              | None -> ()
              | Some (f, r) -> (
                  match f () with
                  | () -> (
                      Promise.resolve r ();
                      aux ()
                    )
                  | exception exn -> (
                      Atomic.set is_okay false;
                      raise exn
                    )
                )
            in
            aux ()
          )
      );
    let runner (f : unit task) : unit =
      let promise, resolver = Promise.create () in
      Stream.add s (Some (f, resolver));
      Promise.await promise
    in
    let check _ = Atomic.get is_okay in
    let dispose _ =
      Stream.add s None
    in
    (runner, { check; dispose })
  in
  {
    sw;
    runners : runner Pool.t = Pool.create ~alloc max_domains;
  }

let async (t : t) (f : unit task) =
  Pool.async ~sw:t.sw t.runners (fun runner ->
      runner f
    )

let async_promise (t : t) (f : 'a task) : 'a Promise.or_exn =
  let promise, resolver = Promise.create () in
  async t (fun () ->
      match f () with
      | x -> Promise.resolve_ok resolver x
      | exception exn -> Promise.resolve_error resolver exn);
  promise

let run t (f : 'a task) : 'a =
  match Promise.await (async_promise t f) with
  | Ok x -> x
  | Error exn -> raise exn

let clear t =
  Pool.clear t.runners
