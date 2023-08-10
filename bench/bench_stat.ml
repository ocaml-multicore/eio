open Eio

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

  let rec make random fs = function
  | Dir { name; perm; children } ->
    let dir = fs / name in
    Path.mkdir ~perm dir;
    Fiber.List.iter (make random dir) children
  | File { name; size; perm } ->
    Path.with_open_out ~create:(`If_missing perm) (fs / name) @@ fun oc ->
    let buf = Cstruct.create (Int64.to_int size) in
    Flow.read_exact random buf;
    Flow.copy (Flow.cstruct_source [ buf ]) oc
end

let with_bench_dir ~random ~fs t fn =
  let dir = Filename.temp_dir "eio-bench-" "-stat" in
  let root = fs / dir in
  Bench_dir.make random root t;
  Fun.protect ~finally:(fun () -> assert (0 = Sys.command ("rm -r " ^ (Path.native_exn root)))) (fun () -> fn root)

let rec bench_stat dir =
  let kind, perm, size =
    Path.stat ~follow:false dir File.[ Kind; Perm; Size ] @@ fun kind perm size ->
    (kind, perm, size)
  in
  match kind with
  | `Directory ->
    let children = Path.read_dir dir |> Fiber.List.map (fun f -> bench_stat (dir / f)) in
    let name = Path.native_exn dir |> Filename.basename in
    Bench_dir.Dir { name; perm; children }
  | `Regular_file ->
    let name = Path.native_exn dir |> Filename.basename in
    File { name; perm; size }
  | _ -> assert false

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

let run_bench ~n ~levels ~random ~fs ~clock =
  let dir = random_bench_dir ~levels ~n |> Bench_dir.sort in
  Eio.traceln "Going to create %i files and directories" (Bench_dir.size dir);
  let bench () =
    with_bench_dir ~random ~fs dir @@ fun root ->
    Eio.traceln "Created %i files and directories" (Bench_dir.size dir);
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
  [ 
    Metric.create "stat-time" (`Float (1e3 *. time)) "ms" (Fmt.str "Time to stat %i files and directories" (Bench_dir.size dir));
    Metric.create "stat-minor" (`Float (1e-3 *. minor)) "kwords" (Fmt.str "Minor words allocated to stat %i files and directories" (Bench_dir.size dir));
    Metric.create "stat-major" (`Float (1e-3 *. major)) "kwords" (Fmt.str "Major words allocated  %i files and directories" (Bench_dir.size dir))
  ]

let run env =
  let fs = Stdenv.fs env in
  let random = Stdenv.secure_random env in
  let clock = Stdenv.clock env in
  run_bench ~n:20 ~levels:4 ~fs ~random ~clock
  