type 'a task = unit -> 'a

type runner = unit task -> unit

type t = {
  sw : Switch.t;
  runners : runner Pool.t;
}

let create ~sw ~max_domains domain_mgr : t =
  let alloc () : runner * runner Pool.handlers =
    let s : unit task option Stream.t = Stream.create 0 in
    Fiber.fork ~sw (fun () ->
        Domain_manager.run domain_mgr (fun () ->
            let rec aux () =
              match Stream.take s with
              | None -> ()
              | Some f -> (
                  f ();
                  aux ()
                )
            in
            aux ()
          )
      );
    let runner (f : unit task) : unit =
      let promise, resolver = Promise.create () in
      Stream.add s
        (Some
           (fun () ->
              f ();
              Promise.resolve resolver ()));
      Promise.await promise
    in
    let check _ = true in
    let dispose _ =
      Stream.add s None
    in
    (runner, { check; dispose })
  in
  {
    sw;
    runners : runner Pool.t = Pool.create ~alloc max_domains;
  }

let async (t : t) (f : 'a task) =
  let promise, resolver = Promise.create () in
  Pool.async ~sw:t.sw t.runners (fun runner ->
      runner (fun () -> Promise.resolve resolver (f ()));
      Promise.await promise
    )

let run t f =
  Promise.await (async t f)
