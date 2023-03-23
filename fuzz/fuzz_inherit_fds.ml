module I = Eio_unix__Inherit_fds

module S = Set.Make(Int)

let pp f = function
  | `Cloexec x -> Fmt.pf f "close %d" x
  | `Keep x -> Fmt.pf f "keep %d" x

let rec has_duplicates ~seen = function
  | [] -> false
  | (dst, _) :: _ when S.mem dst seen -> true
  | (dst, _) :: xs -> has_duplicates xs ~seen:(S.add dst seen)

let inherit_fds mapping =
  let has_duplicates = has_duplicates ~seen:S.empty mapping in
  let fds = Hashtbl.create 10 in
  mapping |> List.iter (fun (_dst, src) ->
      Hashtbl.add fds src (`Cloexec src);
    );
  match I.plan mapping with
  | exception (Invalid_argument _) -> assert has_duplicates
  | plan ->
    assert (not has_duplicates);
    plan |> List.iter (fun {I.src; dst} ->
      (* Fmt.pr "%d -> %d@." src dst; *)
      let v =
        match Hashtbl.find fds src with
        | `Cloexec x | `Keep x ->
          if dst = -1 then `Cloexec x else `Keep x
      in
      Hashtbl.add fds dst v
    );
  mapping |> List.iter (fun (dst, src) ->
      let v = Hashtbl.find fds dst in
      Crowbar.check_eq ~pp v (`Keep src);
      Hashtbl.remove fds dst;
    );
  fds |> Hashtbl.iter (fun x -> function
      | `Cloexec _ -> ()
      | `Keep _ -> Fmt.failwith "%d should be close-on-exec!" x
    )

let fd = Crowbar.range 10       (* Restrict range to make cycles more likely *)

let () =
  Crowbar.(add_test ~name:"inherit_fds" [list (pair fd fd)] inherit_fds)
