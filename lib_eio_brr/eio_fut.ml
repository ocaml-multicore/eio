let await ?abort fut =
  Eio_js_backend.await
    ~setup:(fun ~resolve ~reject:_ -> Fut.await fut resolve)
    ~cancel:(fun () -> Option.iter (fun f -> f ()) abort)

let await_exn ?abort fut =
  let setup ~resolve ~reject =
    Fut.await fut (fun res ->
        match res with Ok v -> resolve v | Error e -> reject (Jv.Error e))
  in
  Eio_js_backend.await ~setup ~cancel:(fun () ->
      Option.iter (fun f -> f ()) abort)

let make ~sw f =
  let fut, set = Fut.create () in
  Eio.Fiber.fork ~sw (fun () -> set (f ()));
  fut

let make_exn ~sw f =
  let fut, set = Fut.create () in
  Eio.Fiber.fork ~sw (fun () ->
      try set (Ok (f ())) with Jv.Error err -> set (Error err));
  fut
