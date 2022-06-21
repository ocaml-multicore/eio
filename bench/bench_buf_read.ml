module R = Eio.Buf_read

let test_data = String.init 100_000_000 (fun _ -> 'x')

let () =
  let r = R.of_string test_data in
  let t0 = Unix.gettimeofday () in
  let i = ref 0 in
  try
    while true do
      assert (R.any_char r = 'x');
      incr i
    done
  with End_of_file ->
    let t1 = Unix.gettimeofday () in
    Eio.traceln "Read %d bytes in %.3fs" !i (t1 -. t0)
