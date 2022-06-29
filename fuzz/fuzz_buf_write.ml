(* Run a random sequence of write operations on an [Eio.Buf_write].
   Check that the expected data gets written to the flow. *)

module W = Eio.Buf_write

let initial_size = 10

type op = Op : string * (W.t -> unit) -> op     (* Expected string, writer *)

let cstruct =
  Crowbar.(map [bytes; int; int]) (fun s off len ->
      if String.length s = 0 then Cstruct.empty
      else (
        let off = min (abs off) (String.length s) in
        let len = min (abs len) (String.length s - off) in
        Cstruct.of_string s ~off ~len
      )
    )

let op =
  let label (name, gen) = Crowbar.with_printer (fun f (Op (s, _)) -> Fmt.pf f "%s:%S" name s) gen in
  Crowbar.choose @@ List.map label [
    "string", Crowbar.(map [bytes]) (fun s -> Op (s, (fun t -> W.string t s)));
    "cstruct", Crowbar.(map [cstruct]) (fun cs -> Op (Cstruct.to_string cs, (fun t -> W.cstruct t cs)));
    "schedule_cstruct", Crowbar.(map [cstruct]) (fun cs -> Op (Cstruct.to_string cs, (fun t -> W.schedule_cstruct t cs)));
    "yield", Crowbar.const @@ Op ("", (fun _ -> Eio.Fiber.yield ()));
    "flush", Crowbar.const @@ Op ("", W.flush);
    "pause", Crowbar.const @@ Op ("", W.pause);
    "unpause", Crowbar.const @@ Op ("", W.unpause);
  ]

let random ops close =
  Eio_mock.Backend.run @@ fun _ ->
  let b = Buffer.create 100 in
  let flow = Eio.Flow.buffer_sink b in
  let expected = ref [] in
  W.with_flow flow ~initial_size (fun t ->
      let perform (Op (s, write)) =
        expected := s :: !expected;
        write t
      in
      List.iter perform ops;
      if close then W.close t
    );
  let expected = String.concat "" (List.rev !expected) in
  Crowbar.check_eq ~pp:Fmt.Dump.string (Buffer.contents b) expected

let () =
  Crowbar.(add_test ~name:"random ops" [list op; bool] random)
