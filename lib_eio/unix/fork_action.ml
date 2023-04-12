type c_action = Obj.t

type t = { run : 'a. ((c_action -> 'a) -> 'a) } [@@unboxed]

(* A [fork_fn] is a C function that can be executed after forking. It cannot call OCaml code or
   run the OCaml GC. It is passed a [Unix.file_descr] for errors and a pointer
   to a [c_action]. On success it should write nothing to the error stream and
   return 0. On error, it should write a message to the error FD and return a
   non-zero value for the exit status (e.g. 1). *)
type fork_fn

let rec with_actions actions fn =
  match actions with
  | [] -> fn []
  | { run } :: xs ->
    run @@ fun c_action ->
    with_actions xs @@ fun c_actions ->
    fn (c_action :: c_actions)

external action_execve : unit -> fork_fn = "eio_unix_fork_execve"
let action_execve = action_execve ()
let execve path ~argv ~env = { run = fun k -> k (Obj.repr (action_execve, path, argv, env)) }

external action_chdir : unit -> fork_fn = "eio_unix_fork_chdir"
let action_chdir = action_chdir ()
let chdir path = { run = fun k -> k (Obj.repr (action_chdir, path)) }

external action_fchdir : unit -> fork_fn = "eio_unix_fork_fchdir"
let action_fchdir = action_fchdir ()
let fchdir fd = {
  run = fun k ->
    Fd.use_exn "fchdir" fd @@ fun fd ->
    k (Obj.repr (action_fchdir, fd)) }

let int_of_fd : Unix.file_descr -> int = Obj.magic

type action = Inherit_fds.action = { src : int; dst : int }

let rec with_fds mapping k =
  match mapping with
  | [] -> k []
  | (dst, src, _) :: xs ->
    Fd.use_exn "inherit_fds" src @@ fun src ->
    with_fds xs @@ fun xs ->
    k ((dst, int_of_fd src) :: xs)

type blocking = [
  | `Blocking
  | `Nonblocking
  | `Preserve_blocking
]

external action_dups : unit -> fork_fn = "eio_unix_fork_dups"
let action_dups = action_dups ()
let inherit_fds m =
  let blocking = m |> List.filter_map (fun (dst, _, flags) ->
      match flags with
      | `Blocking -> Some (dst, true)
      | `Nonblocking -> Some (dst, false)
      | `Preserve_blocking -> None
    )
  in
  with_fds m @@ fun m ->
  let plan : action list = Inherit_fds.plan m in
  { run = fun k -> k (Obj.repr (action_dups, plan, blocking)) }
