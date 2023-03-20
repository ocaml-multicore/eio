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

let err_closed op () =
  Fmt.failwith "%s: FD is closed!" op

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
    Rcfd.use ~if_closed:(err_closed "fchdir") fd @@ fun fd ->
    k (Obj.repr (action_fchdir, fd)) }
