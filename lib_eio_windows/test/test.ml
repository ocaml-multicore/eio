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

let () =
  Eio_windows.run @@ fun env ->
  Alcotest.run "eio_windows" [
    "net", Test_net.tests env;
    "timeout", Timeout.tests env;
    "random", Random.tests env
  ]