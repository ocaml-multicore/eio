open Eio.Std

module Path = Eio.Path

let () = Random.init 3

let ( / ) = Eio.Path.( / )

module Bench_dir = struct
  type t = 
    | Dir of { name : string; perm : int; children : t list }
    | File of { name : string; size : int64; perm : int; }

  let get_name = function Dir { name; _ } | File { name; _ } -> name

  let get_children = function
    | Dir { children; _ } -> children
    | File _ -> invalid_arg "Files don't have children"

  let compare a b = String.compare (get_name a) (get_name b)

  let rec sort = function
    | Dir ({ children; _ } as v) ->
      let c = List.map sort children in
      let c = List.stable_sort compare c in
      Dir { v with children = c }
    | File _ as f -> f

  let rec size = function
    | Dir { children; _ } ->
      List.fold_left (fun acc v -> acc + size v) 0 children
    | File _ -> 1

  let rec pp ppf = function
    | Dir { name; perm; children } ->
      if children = [] then Fmt.pf ppf "dir %s (0o%o)" name perm else
        Fmt.pf ppf "@[<v2>dir %s (0o%o)@ %a@]" name perm Fmt.(list ~sep:Fmt.cut pp) children
    | File { name; size; perm } ->
      Fmt.pf ppf "file %s (0o%o) %Lu" name perm size

  let make random fs t =
    let limit = Eio.Semaphore.make 32 in        (* Prevent FD exhaustion *)
    let rec aux fs = function
      | Dir { name; perm; children } ->
        let dir = fs / name in
        Eio.Semaphore.acquire limit;
        Path.mkdir ~perm dir;
        Eio.Semaphore.release limit;
        Fiber.List.iter (aux dir) children
      | File { name; size; perm } ->
        Eio.Semaphore.acquire limit;
        let buf = Cstruct.create (Int64.to_int size) in
        Eio.Flow.read_exact random buf;
        Path.with_open_out ~create:(`If_missing perm) (fs / name) (fun oc ->
            Eio.Flow.write oc [ buf ]
          );
        Eio.Semaphore.release limit
    in
    aux fs t
end

let with_tmp_dir ~fs prefix suffix fn =
  Switch.run @@ fun sw ->
  let dir = fs / Filename.temp_dir prefix suffix in
  Switch.on_release sw (fun () -> Path.rmtree dir);
  fn dir

let bench_stat root =
  let limit = Eio.Semaphore.make 32 in        (* Prevent FD exhaustion *)
  let rec aux dir =
    Eio.Semaphore.acquire limit;
    let { Eio.File.Stat.kind; perm; size; _ } = Path.stat ~follow:false dir in
    match kind with
    | `Directory ->
      let items = Path.read_dir dir in
      Eio.Semaphore.release limit;
      let children = items |> Fiber.List.map (fun f -> aux (dir / f)) in
      let name = Path.native_exn dir |> Filename.basename in
      Bench_dir.Dir { name; perm; children }
    | `Regular_file ->
      Eio.Semaphore.release limit;
      let name = Path.native_exn dir |> Filename.basename in
      File { name; perm; size = Optint.Int63.to_int64 size }
    | _ -> assert false
  in
  aux root

let file name = Bench_dir.File { name; perm = 0o644; size = 128L }
let dir name children = Bench_dir.Dir { name; perm = 0o700; children }

let random_bench_dir ~n ~levels =
  if levels < 0 then invalid_arg "Levels should be > 0";
  let rec loop root = function
    | 1 -> (
        match root with
        | Bench_dir.Dir d ->
          let leaf_files = List.init n (fun i -> file (Fmt.str "test-file-%i-%i" 1 i)) in
          Bench_dir.Dir { d with children = leaf_files }
        | _ -> failwith "Root is always expected to be a directory"
      )
    | level ->
      match root with
      | Bench_dir.Dir d ->
        let files = List.init n (fun i -> file (Fmt.str "test-file-%i-%i" level i)) in
        let dirs = List.init n (fun i -> dir (Fmt.str "test-dir-%i-%i" level i) []) in 
        let dirs = List.map (fun dir -> loop dir (level - 1)) dirs in
        Bench_dir.Dir { d with children = dirs @ files }
      | _ -> failwith "Root is always expected to be directory"
  in
  loop (dir "root" []) levels

let run_bench ~n ~levels ~random ~root ~clock =
  let dir = random_bench_dir ~levels ~n |> Bench_dir.sort in
  traceln "Going to create %i files and directories" (Bench_dir.size dir);
  let create_time =
    let t0 = Eio.Time.now clock in
    Bench_dir.make random root dir;
    let t1 = Eio.Time.now clock in
    t1 -. t0
  in
  traceln "Created in %.2f s" create_time;
  let bench () =
    Gc.full_major ();
    let stat0 = Gc.stat () in
    let t0 = Eio.Time.now clock in
    let res = bench_stat root in
    let t1 = Eio.Time.now clock in
    let stat1 = Gc.stat () in
    match Bench_dir.sort res with
    | Dir { children = [ dir' ]; _ } ->
      assert (dir = dir');
      let time_total = t1 -. t0 in
      let minor_total = stat1.minor_words -. stat0.minor_words in
      let major_total = stat1.major_words -. stat0.major_words in
      time_total, minor_total, major_total
    | _ -> failwith "Stat not the same as the spec"
  in
  let time, minor, major = bench () in
  traceln "Statted in %.2f s" time;
  let remove_time =
    let t0 = Eio.Time.now clock in
    Eio.Path.read_dir root |> List.iter (fun item -> Eio.Path.rmtree (root / item));
    let t1 = Eio.Time.now clock in
    t1 -. t0
  in
  traceln "Removed in %.2f s" remove_time;
  [ 
    Metric.create "create-time" (`Float (1e3 *. create_time)) "ms" (Fmt.str "Time to create %i files and directories" (Bench_dir.size dir));
    Metric.create "stat-time" (`Float (1e3 *. time)) "ms" (Fmt.str "Time to stat %i files and directories" (Bench_dir.size dir));
    Metric.create "stat-minor" (`Float (1e-3 *. minor)) "kwords" (Fmt.str "Minor words allocated to stat %i files and directories" (Bench_dir.size dir));
    Metric.create "stat-major" (`Float (1e-3 *. major)) "kwords" (Fmt.str "Major words allocated  %i files and directories" (Bench_dir.size dir));
    Metric.create "remove-time" (`Float (1e3 *. remove_time)) "ms" "Time to remove everything";
  ]

let run env =
  let fs = Eio.Stdenv.fs env in
  let random = Eio.Stdenv.secure_random env in
  let clock = Eio.Stdenv.clock env in
  with_tmp_dir ~fs "eio-bench-" "-stat" @@ fun root ->
  run_bench ~n:20 ~levels:4 ~root ~random ~clock
