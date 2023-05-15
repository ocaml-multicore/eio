type 'a task = unit -> 'a

type runner = unit task -> unit

type t = {
  runners : runner Pool.t;
}

let create ~max_domains domain_mgr : t =
  let alloc () : runner =
    let s : unit task Stream.t = Stream.create 0 in
    Switch.run (fun sw ->
        Fiber.fork ~sw (fun () ->
            Domain_manager.run domain_mgr (fun () ->
                let rec aux () =
                  let f = Stream.take s in
                  f ();
                  aux ()
                in
                aux ()
              )
          )
      );
    (fun (f : unit task) : unit ->
       let promise, resolver = Promise.create () in
       Stream.add s (fun () ->
           f ();
           Promise.resolve resolver ());
       Promise.await promise
    )
  in
  {
    runners : runner Pool.t = Pool.create ~alloc max_domains;
  }

let async (t : t) (f : 'a task) =
  let promise, resolver = Promise.create () in
  Pool.async t.runners (fun runner ->
      runner (fun () -> Promise.resolve resolver (f ()));
      Promise.await promise
    )

let run t f =
  Promise.await (async t f)
