module Timeout = struct
  let test clock () =
    let t0 = Unix.gettimeofday () in
    Eio.Time.sleep clock 0.01;
    let t1 = Unix.gettimeofday () in
    let diff = t1 -. t0 in
    if diff >= 0.01 then () else Alcotest.failf "Expected bigger difference than %f" diff


  let tests env = [
    "timeout", `Quick, test env#clock
  ]
end

module Random = struct
  let test_random env () =
    let src = Eio.Stdenv.secure_random env in
    let b1 = Cstruct.create 8 in
    let b2 = Cstruct.create 8 in
    Eio.Flow.read_exact src b1;
    Eio.Flow.read_exact src b2;
    Alcotest.(check bool) "different random" (not (Cstruct.equal b1 b2)) true

  let tests env = [
    "different", `Quick, test_random env
  ]
end

module Dla = struct

  let test_dla () =
    let open Kcas in
    let x = Loc.make 0 in
    let y = Loc.make 0 in
    let foreign_domain = Domain.spawn @@ fun () ->
      let x = Loc.get_as (fun x -> Retry.unless (x <> 0); x) x in
      Loc.set y 22;
      x
    in
    Loc.set x 20;
    let y' = Loc.get_as (fun y -> Retry.unless (y <> 0); y) y in
    Alcotest.(check int) "correct y" y' 22;
    let ans = y' + Domain.join foreign_domain in
    Alcotest.(check int) "answer" ans 42

  let tests = [
    "dla", `Quick, test_dla
  ]
end


let () =
  Eio_windows.run @@ fun env ->
  Alcotest.run ~bail:true "eio_windows" [
    "net", Test_net.tests env;
    "fs", Test_fs.tests env;
    "timeout", Timeout.tests env;
    "random", Random.tests env;
    "dla", Dla.tests
  ]