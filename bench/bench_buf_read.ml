module R = Eio.Buf_read

let run _env =
  let test_data = String.make 100_000_000 'x' in
  let r = R.of_string test_data in
  let t0 = Unix.gettimeofday () in
  let i = ref 0 in
  try
    while true do
      assert (R.any_char r = 'x');
      incr i
    done;
    assert false
  with End_of_file ->
    let t1 = Unix.gettimeofday () in
    let time = t1 -. t0 in
    let bytes_per_second = float (String.length test_data) /. time in
    [Metric.create "any_char" (`Float bytes_per_second) "bytes/s" "Parsing a long string one character at a time"]
