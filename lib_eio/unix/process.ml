open Eio.Std

let resolve_program name =
  if Filename.is_implicit name then (
    Sys.getenv_opt "PATH"
    |> Option.value ~default:"/bin:/usr/bin"
    |> String.split_on_char ':'
    |> List.find_map (fun dir ->
        let p = Filename.concat dir name in
        if Sys.file_exists p then Some p else None
      )
  ) else if Sys.file_exists name then (
    Some name
  ) else None

let read_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some f ->
    match Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy f w;
          Eio.Flow.close w
        );
      let r = Resource.fd r in
      to_close := r :: !to_close;
      r

let write_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some f ->
    match Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Private.pipe sw in
      Fiber.fork ~sw (fun () ->
          Eio.Flow.copy r f;
          Eio.Flow.close r
        );
      let w = Resource.fd w in
      to_close := w :: !to_close;
      w

let with_close_list fn =
  let to_close = ref [] in
  let close () =
    List.iter Fd.close !to_close
  in
  match fn to_close with
  | x -> close (); x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    close ();
    Printexc.raise_with_backtrace ex bt

let get_executable ~args = function
  | Some exe -> exe
  | None ->
    match args with
    | [] -> invalid_arg "Arguments list is empty and no executable given!"
    | (x :: _) ->
      match resolve_program x with
      | Some x -> x
      | None -> raise (Eio.Process.err (Executable_not_found x))

let get_env = function
  | Some e -> e
  | None -> Unix.environment ()

type ty = [ `Generic | `Unix ] Eio.Process.ty
type 'a t = ([> ty] as 'a) r

type mgr_ty = [`Generic | `Unix] Eio.Process.mgr_ty
type 'a mgr = ([> mgr_ty] as 'a) r

module Pi = struct
  module type MGR = sig
    include Eio.Process.Pi.MGR

    val spawn_unix :
      t ->
      sw:Switch.t ->
      ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
      env:string array ->
      fds:(int * Fd.t * Fork_action.blocking) list ->
      executable:string ->
      string list ->
      ty r
  end

  type (_, _, _) Eio.Resource.pi +=
    | Mgr_unix : ('t, (module MGR with type t = 't), [> mgr_ty]) Eio.Resource.pi

  let mgr_unix (type t tag) (module X : MGR with type t = t and type tag = tag) =
    Eio.Resource.handler [
      H (Eio.Process.Pi.Mgr, (module X));
      H (Mgr_unix, (module X));
    ]
end

module Make_mgr (X : sig
  type t

  val spawn_unix :
    t ->
    sw:Switch.t ->
    ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    fds:(int * Fd.t * Fork_action.blocking) list ->
    executable:string ->
    string list ->
    ty r
end) = struct
  type t = X.t

  type tag = [ `Generic | `Unix ]

  let pipe _ ~sw =
    (Private.pipe sw :> ([Eio.Resource.close_ty | Eio.Flow.source_ty] r *
    [Eio.Resource.close_ty | Eio.Flow.sink_ty] r))

  let spawn v ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    let executable = get_executable executable ~args in
    let env = get_env env in
    with_close_list @@ fun to_close ->
    let stdin_fd  = read_of_fd  ~sw stdin  ~default:Fd.stdin  ~to_close in
    let stdout_fd = write_of_fd ~sw stdout ~default:Fd.stdout ~to_close in
    let stderr_fd = write_of_fd ~sw stderr ~default:Fd.stderr ~to_close in
    let fds = [
      0, stdin_fd, `Blocking;
      1, stdout_fd, `Blocking;
      2, stderr_fd, `Blocking;
    ] in
    X.spawn_unix v ~sw ?cwd ~env ~fds ~executable args

  let spawn_unix = X.spawn_unix
end

let spawn_unix ~sw (Eio.Resource.T (v, ops)) ?cwd ~fds ?env ?executable args =
  let module X = (val (Eio.Resource.get ops Pi.Mgr_unix)) in
  let executable = get_executable executable ~args in
  let env = get_env env in
  X.spawn_unix v ~sw ?cwd ~fds ~env ~executable args

let sigchld = Eio.Condition.create ()

let install_sigchld_handler () =
  Sys.(set_signal sigchld) (Signal_handle (fun (_:int) -> Eio.Condition.broadcast sigchld))
