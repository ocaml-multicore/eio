(*
 * Copyright (C) 2021 Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "eio_luv" ~doc:"Eio backend using luv"
module Log = (val Logs.src_log src : Logs.LOG)

open Eio.Std
open Obj.Effect_handlers 
open Obj.Effect_handlers.Deep 

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

type 'a or_error = ('a, Luv.Error.t) result

exception Luv_error of Luv.Error.t

let () =
  Printexc.register_printer @@ function
  | Luv_error e -> Some (Printf.sprintf "Eio_luv.Luv_error(%s) (* %s *)" (Luv.Error.err_name e) (Luv.Error.strerror e))
  | _ -> None

let wrap_error ~path e =
  let ex = Luv_error e in
  match e with
  | `EEXIST -> Eio.Dir.Already_exists (path, ex)
  | `ENOENT -> Eio.Dir.Not_found (path, ex)
  | _ -> ex

let or_raise = function
  | Ok x -> x
  | Error e -> raise (Luv_error e)

let or_raise_path path = function
  | Ok x -> x
  | Error e -> raise (wrap_error ~path e)

module Suspended = struct
  type 'a t = {
    tid : Ctf.id;
    k : ('a, unit) continuation;
  }

  let continue t v =
    Ctf.note_switch t.tid;
    continue t.k v

  let discontinue t ex =
    Ctf.note_switch t.tid;
    discontinue t.k ex

  let continue_result t = function
    | Ok x -> continue t x
    | Error x -> discontinue t x
end

type _ eff += Await : (('a -> unit) -> unit) -> 'a eff
let await fn = perform (Await fn)

type _ eff += Enter : ('a Suspended.t -> unit) -> 'a eff
let enter fn = perform (Enter fn)

let await_exn fn =
  perform (Await fn) |> or_raise

let enqueue_thread k v =
  let yield = Luv.Timer.init () |> or_raise in
  Luv.Timer.start yield 0 (fun () -> Suspended.continue k v) |> or_raise

let enqueue_result_thread k r =
  let yield = Luv.Timer.init () |> or_raise in
  Luv.Timer.start yield 0 (fun () -> Suspended.continue_result k r) |> or_raise

let enqueue_failed_thread k ex =
  let yield = Luv.Timer.init () |> or_raise in
  Luv.Timer.start yield 0 (fun () -> Suspended.discontinue k ex) |> or_raise

let yield ?sw () =
  Option.iter Switch.check sw;
  enter @@ fun k ->
  enqueue_thread k ()

let with_cancel ?sw ~request fn =
  let cancel = Switch.add_cancel_hook_opt sw (fun _ ->
      match Luv.Request.cancel request with
      | Ok () -> ()
      | Error e -> Log.debug (fun f -> f "Cancel failed: %s" (Luv.Error.strerror e))
    ) in
  Fun.protect fn ~finally:(fun () -> Switch.remove_hook cancel)

module Handle = struct
  type 'a t = {
    mutable release_hook : Switch.hook;        (* Use this on close to remove switch's [on_release] hook. *)
    mutable fd : [`Open of 'a Luv.Handle.t | `Closed]
  }

  let get op = function
    | { fd = `Open fd; _ } -> fd
    | { fd = `Closed ; _ } -> invalid_arg (op ^ ": handle used after calling close!")

  let is_open = function
    | { fd = `Open _; _ } -> true
    | { fd = `Closed; _ } -> false

  let close t =
    Ctf.label "close";
    let fd = get "close" t in
    t.fd <- `Closed;
    Switch.remove_hook t.release_hook;
    enter @@ fun k ->
    Luv.Handle.close fd (Suspended.continue k)

  let ensure_closed t =
    if is_open t then close t

  let to_luv x = get "to_luv" x

  let of_luv_no_hook fd =
    { fd = `Open fd; release_hook = Switch.null_hook }

  let of_luv ~sw fd =
    let t = of_luv_no_hook fd in
    t.release_hook <- Switch.on_release_cancellable sw (fun () -> ensure_closed t);
    t
end

module File = struct
  type t = {
    mutable release_hook : Switch.hook;        (* Use this on close to remove switch's [on_release] hook. *)
    mutable fd : [`Open of Luv.File.t | `Closed]
  }

  let get op = function
    | { fd = `Open fd; _ } -> fd
    | { fd = `Closed ; _ } -> invalid_arg (op ^ ": file descriptor used after calling close!")

  let is_open = function
    | { fd = `Open _; _ } -> true
    | { fd = `Closed; _ } -> false

  let close t =
    Ctf.label "close";
    let fd = get "close" t in
    t.fd <- `Closed;
    Switch.remove_hook t.release_hook;
    await_exn (Luv.File.close fd)

  let ensure_closed t =
    if is_open t then close t

  let to_luv = get "to_luv"

  let of_luv_no_hook fd =
    { fd = `Open fd; release_hook = Switch.null_hook }

  let of_luv ~sw fd =
    let t = of_luv_no_hook fd in
    t.release_hook <- Switch.on_release_cancellable sw (fun () -> ensure_closed t);
    t

  let open_ ~sw ?mode path flags =
    let request = Luv.File.Request.make () in
    with_cancel ~sw ~request @@ fun () ->
    await (Luv.File.open_ ?mode ~request path flags) |> Result.map (of_luv ~sw)

  let read ?sw fd bufs =
    let request = Luv.File.Request.make () in
    with_cancel ?sw ~request @@ fun () ->
    await (Luv.File.read ~request (get "read" fd) bufs)

  let rec write ?sw fd bufs =
    let request = Luv.File.Request.make () in
    with_cancel ?sw ~request @@ fun () ->
    let sent = await_exn (Luv.File.write ~request (get "write" fd) bufs) in
    let rec aux = function
      | [] -> ()
      | x :: xs when Luv.Buffer.size x = 0 -> aux xs
      | bufs -> write ?sw fd bufs
    in
    aux @@ Luv.Buffer.drop bufs (Unsigned.Size_t.to_int sent)

  let realpath ?sw path =
    let request = Luv.File.Request.make () in
    with_cancel ?sw ~request @@ fun () ->
    await (Luv.File.realpath ~request path)

  let mkdir ?sw ~mode path =
    let request = Luv.File.Request.make () in
    with_cancel ?sw ~request @@ fun () ->
    await (Luv.File.mkdir ~request ~mode path)
end

module Stream = struct
  type 'a t = [`Stream of 'a] Handle.t

  let rec read_into ?sw (sock:'a t) buf =
    Option.iter Switch.check sw;
    let r = enter (fun k ->
        let cancel = Switch.add_cancel_hook_opt sw (fun ex ->
            Luv.Stream.read_stop (Handle.get "read_into:cancel" sock) |> or_raise;
            enqueue_failed_thread k (Switch.Cancelled ex)
          ) in
        Luv.Stream.read_start (Handle.get "read_start" sock) ~allocate:(fun _ -> buf) (fun r ->
            Switch.remove_hook cancel;
            Luv.Stream.read_stop (Handle.get "read_stop" sock) |> or_raise;
            Suspended.continue k r
          )
      ) in
    match r with
    | Ok buf' ->
      let len = Luv.Buffer.size buf' in
      if len > 0 then len
      else read_into ?sw sock buf       (* Luv uses a zero-length read to mean EINTR! *)
    | Error `EOF -> raise End_of_file
    | Error (`ECONNRESET as e) -> raise (Eio.Net.Connection_reset (Luv_error e))
    | Error x -> raise (Luv_error x)

  let rec skip_empty = function
    | empty :: xs when Luv.Buffer.size empty = 0 -> skip_empty xs
    | xs -> xs

  let rec write ?sw t bufs =
    let err, n = 
      (* note: libuv doesn't seem to allow cancelling stream writes *)
      enter (fun k ->
          Luv.Stream.write (Handle.get "write_stream" t) bufs @@ fun err n ->
          Suspended.continue k (err, n)
        )
    in
    or_raise err;
    match Luv.Buffer.drop bufs n |> skip_empty with
    | [] -> ()
    | bufs -> write ?sw t bufs
end

let sleep_until ?sw due =
  Option.iter Switch.check sw;
  let delay = 1000. *. (due -. Unix.gettimeofday ()) |> ceil |> truncate |> max 0 in
  let timer = Luv.Timer.init () |> or_raise in
  enter @@ fun k ->
  let cancel = Switch.add_cancel_hook_opt sw (fun ex ->
      Luv.Timer.stop timer |> or_raise;
      Luv.Handle.close timer (fun () -> ());
      enqueue_failed_thread k ex
    ) in
  Luv.Timer.start timer delay (fun () ->
      Switch.remove_hook cancel;
      Suspended.continue k ()
    ) |> or_raise

let run_compute fn =
  match_with fn ()
  { retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e: a eff) -> 
      match e with 
      | Eio.Private.Effects.Trace -> 
        Some (fun (k : (a,_) continuation) -> continue k Eunix.Trace.default_traceln)
      | _ -> None
  }

module Objects = struct
  type _ Eio.Generic.ty += FD : File.t Eio.Generic.ty

  type has_fd = < fd : File.t >
  type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
  type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

  let get_fd (t : <has_fd; ..>) = t#fd

  let get_fd_opt t = Eio.Generic.probe t FD

  let flow fd = object (_ : <source; sink; ..>)
    method fd = fd
    method close = File.close fd

    method probe : type a. a Eio.Generic.ty -> a option = function
      | FD -> Some fd
      | _ -> None

    method read_into ?sw buf =
      let buf = Cstruct.to_bigarray buf in
      match File.read ?sw fd [buf] |> or_raise |> Unsigned.Size_t.to_int with
      | 0 -> raise End_of_file
      | got -> got

    method read_methods = []

    method write ?sw src =
      let buf = Luv.Buffer.create 4096 in
      try
        while true do
          let got = Eio.Flow.read_into src (Cstruct.of_bigarray buf) in
          let sub = Luv.Buffer.sub buf ~offset:0 ~length:got in
          File.write ?sw fd [sub]
        done
      with End_of_file -> ()
  end

  let source fd = (flow fd :> source)
  let sink   fd = (flow fd :> sink)

  let socket sock = object
    inherit Eio.Flow.two_way

    method read_into ?sw buf =
      let buf = Cstruct.to_bigarray buf in
      Stream.read_into ?sw sock buf

    method read_methods = []

    method write ?sw src =
      let buf = Luv.Buffer.create 4096 in
      try
        while true do
          let got = Eio.Flow.read_into src (Cstruct.of_bigarray buf) in
          let buf' = Luv.Buffer.sub buf ~offset:0 ~length:got in
          Stream.write ?sw sock [buf']
        done
      with End_of_file -> ()

    method close =
      Handle.close sock

    method shutdown = function
      | `Send -> await_exn @@ Luv.Stream.shutdown (Handle.get "shutdown" sock)
      | `Receive -> failwith "shutdown receive not supported"
      | `All ->
        Log.warn (fun f -> f "shutdown receive not supported");
        await_exn @@ Luv.Stream.shutdown (Handle.get "shutdown" sock)
  end

  class virtual ['a] listening_socket ~backlog sock = object (self)
    inherit Eio.Net.listening_socket

    val ready = Eio.Semaphore.make 0

    method private virtual make_client : 'a Luv.Stream.t
    method private virtual get_client_addr : 'a Stream.t -> Eio.Net.Sockaddr.t

    method close = Handle.close sock

    method accept_sub ~sw ~on_error fn =
      Eio.Semaphore.acquire ~sw ready;
      let client = self#make_client |> Handle.of_luv_no_hook in
      match Luv.Stream.accept ~server:(Handle.get "accept" sock) ~client:(Handle.get "accept" client) with
      | Error e ->
        Handle.close client;
        raise (Luv_error e)
      | Ok () ->
        Fibre.fork_sub_ignore ~sw ~on_error
          (fun sw ->
             let client_addr = self#get_client_addr client in
             fn ~sw (socket client :> <Eio.Flow.two_way; Eio.Flow.close>) client_addr
          )
          ~on_release:(fun () -> Handle.ensure_closed client)

    initializer
      Luv.Stream.listen ~backlog (Handle.get "listen" sock) (fun x ->
          or_raise x;
          Eio.Semaphore.release ready
        )
  end

  (* TODO: implement, or maybe remove from the Eio API.
     Luv makes TCP sockets reuse_addr by default, and maybe that's fine everywhere.
     Extracting the FD will require https://github.com/aantron/luv/issues/120 *)
  let luv_reuse_addr _sock _v = ()

  (* This is messy. Should make a generic sockaddr type for eio. *)
  let luv_addr_of_unix host port =
    let host = Unix.string_of_inet_addr host in
    match Luv.Sockaddr.ipv6 host port with
    | Ok addr -> addr
    | Error _ -> Luv.Sockaddr.ipv4 host port |> or_raise

  let luv_ip_addr_to_unix addr =
    let host = Luv.Sockaddr.to_string addr |> Option.get in
    let port = Luv.Sockaddr.port addr |> Option.get in
    (Unix.inet_addr_of_string host, port)

  let listening_ip_socket ~backlog sock = object
    inherit [[ `TCP ]] listening_socket ~backlog sock

    method private make_client = Luv.TCP.init () |> or_raise
    method private get_client_addr c =
      `Tcp (Luv.TCP.getpeername (Handle.get "get_client_addr" c) |> or_raise |> luv_ip_addr_to_unix)
  end

  let listening_unix_socket ~backlog sock = object
    inherit [[ `Pipe ]] listening_socket ~backlog sock

    method private make_client = Luv.Pipe.init () |> or_raise
    method private get_client_addr c =
      `Unix (Luv.Pipe.getpeername (Handle.get "get_client_addr" c) |> or_raise)
  end

  let net = object
    inherit Eio.Net.t

    method listen ~reuse_addr ~backlog ~sw = function
      | `Tcp (host, port) ->
        let sock = Luv.TCP.init () |> or_raise |> Handle.of_luv ~sw in
        luv_reuse_addr sock reuse_addr;
        let addr = luv_addr_of_unix host port in
        Luv.TCP.bind (Handle.get "bind" sock) addr |> or_raise;
        listening_ip_socket ~backlog sock
      | `Unix path         ->
        let sock = Luv.Pipe.init () |> or_raise |> Handle.of_luv ~sw in
        luv_reuse_addr sock reuse_addr;
        if reuse_addr then (
          match Unix.lstat path with
          | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
          | _ -> ()
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        );
        Luv.Pipe.bind (Handle.get "bind" sock) path |> or_raise;
        (* Remove the path when done (except for abstract sockets). *)
        if String.length path > 0 && path.[0] <> Char.chr 0 then
          Switch.on_release sw (fun () -> Unix.unlink path);
        listening_unix_socket ~backlog sock

    (* todo: how do you cancel connect operations with luv? *)
    method connect ~sw = function
      | `Tcp (host, port) ->
        let sock = Luv.TCP.init () |> or_raise |> Handle.of_luv ~sw in
        let addr = luv_addr_of_unix host port in
        await_exn (Luv.TCP.connect (Handle.get "connect" sock) addr);
        socket sock
      | `Unix path ->
        let sock = Luv.Pipe.init () |> or_raise |> Handle.of_luv ~sw in
        await_exn (Luv.Pipe.connect (Handle.get "connect" sock) path);
        socket sock
  end

  type stdenv = <
    stdin : source;
    stdout : sink;
    stderr : sink;
    net : Eio.Net.t;
    domain_mgr : Eio.Domain_manager.t;
    clock : Eio.Time.clock;
    fs : Eio.Dir.t;
    cwd : Eio.Dir.t;
  >

  let domain_mgr = object
    inherit Eio.Domain_manager.t

    method run_compute_unsafe (type a) fn =
      let domain_k : (unit Domain.t * a Suspended.t) option ref = ref None in
      let result = ref None in
      let async = Luv.Async.init (fun async ->
          (* This is called in the parent domain after returning to the mainloop,
             so [domain_k] must be set by then. *)
          let domain, k = Option.get !domain_k in
          Domain.join domain;
          Luv.Handle.close async @@ fun () ->
          Suspended.continue_result k (Option.get !result)
        ) |> or_raise
      in
      enter @@ fun k ->
      let d = Domain.spawn (fun () ->
          result := Some (match run_compute fn with
              | v -> Ok v
              | exception ex -> Error ex
            );
          Luv.Async.send async |> or_raise
        ) in
      domain_k := Some (d, k)
  end

  let clock = object
    inherit Eio.Time.clock

    method now = Unix.gettimeofday ()
    method sleep_until = sleep_until
  end

  (* Warning: libuv doesn't provide [openat], etc, and so there is probably no way to make this safe.
     We make a best-efforts attempt to enforce the sandboxing using realpath and [`NOFOLLOW].
     todo: this needs more testing *)
  class dir dir_path = object (self)
    inherit Eio.Dir.t

    (* Resolve a relative path to an absolute one, with no symlinks.
       @raise Eio.Dir.Permission_denied if it's outside of [dir_path]. *)
    method private resolve ?sw path =
      if Filename.is_relative path then (
        let dir_path = File.realpath ?sw dir_path |> or_raise_path dir_path in
        let full = File.realpath ?sw (Filename.concat dir_path path) |> or_raise_path path in
        let prefix_len = String.length dir_path + 1 in
        if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then
          full
        else if full = dir_path then
          full
        else
          raise (Eio.Dir.Permission_denied (path, Failure (Fmt.str "Path %S is outside of sandbox %S" full dir_path)))
      ) else (
        raise (Eio.Dir.Permission_denied (path, Failure (Fmt.str "Path %S is absolute" path)))
      )

    (* We want to create [path]. Check that the parent is in the sandbox. *)
    method private resolve_new ?sw path =
      let dir, leaf = Filename.dirname path, Filename.basename path in
      if leaf = ".." then Fmt.failwith "New path %S ends in '..'!" path
      else match self#resolve ?sw dir with
        | dir -> Filename.concat dir leaf
        | exception Eio.Dir.Permission_denied (dir, ex) ->
          raise (Eio.Dir.Permission_denied (Filename.concat dir leaf, ex))

    method open_in ~sw path =
      let fd = File.open_ ~sw (self#resolve ~sw path) [`NOFOLLOW; `RDONLY] |> or_raise_path path in
      (flow fd :> <Eio.Flow.source; Eio.Flow.close>)

    method open_out ~sw ~append ~create path =
      let mode, flags =
        match create with
        | `Never            -> 0,    []
        | `If_missing  perm -> perm, [`CREAT]
        | `Or_truncate perm -> perm, [`CREAT; `TRUNC]
        | `Exclusive   perm -> perm, [`CREAT; `EXCL]
      in
      let flags = if append then `APPEND :: flags else flags in
      let flags = `RDWR :: `NOFOLLOW :: flags in
      let real_path =
        if create = `Never then self#resolve path
        else self#resolve_new path
      in
      let fd = File.open_ ~sw real_path flags ~mode:[`NUMERIC mode] |> or_raise_path path in
      (flow fd :> <Eio.Dir.rw; Eio.Flow.close>)

    method open_dir ~sw path =
      Switch.check sw;
      new dir (self#resolve ~sw path)

    (* libuv doesn't seem to provide a race-free way to do this. *)
    method mkdir ?sw ~perm path =
      let real_path = self#resolve_new ?sw path in
      File.mkdir ~mode:[`NUMERIC perm] real_path |> or_raise_path path

    method close = ()
  end

  (* Full access to the filesystem. *)
  let fs = object
    inherit dir "/"

    (* No checks *)
    method! private resolve ?sw:_ path = path
  end

  let cwd = object
    inherit dir "."
  end

  let stdenv () =
    let stdin = lazy (source (File.of_luv_no_hook Luv.File.stdin)) in
    let stdout = lazy (sink (File.of_luv_no_hook Luv.File.stdout)) in
    let stderr = lazy (sink (File.of_luv_no_hook Luv.File.stderr)) in
    object (_ : stdenv)
      method stdin  = Lazy.force stdin
      method stdout = Lazy.force stdout
      method stderr = Lazy.force stderr
      method net = net
      method domain_mgr = domain_mgr
      method clock = clock
      method fs = (fs :> Eio.Dir.t)
      method cwd = (cwd :> Eio.Dir.t)
    end
end  

let run main =
  Log.debug (fun l -> l "starting run");
  let stdenv = Objects.stdenv () in
  let rec fork ~tid fn =
    Ctf.note_switch tid;
    match_with fn () 
    { retc = (fun () -> ());
      exnc = (fun e -> raise e);
      effc = fun (type a) (e : a eff) ->
        match e with
        | Await fn ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            fn (Suspended.continue k))
        | Eio.Private.Effects.Trace ->
          Some (fun k -> continue k Eunix.Trace.default_traceln)
        | Eio.Private.Effects.Fork f ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            let id = Ctf.mint_id () in
            Ctf.note_created id Ctf.Task;
            let promise, resolver = Promise.create_with_id id in
            enqueue_thread k promise;
            fork
              ~tid:id
              (fun () ->
                 match f () with
                 | x -> Promise.fulfill resolver x
                 | exception ex ->
                   Log.debug (fun f -> f "Forked fibre failed: %a" Fmt.exn ex);
                   Promise.break resolver ex
              ))
        | Eio.Private.Effects.Fork_ignore f ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            enqueue_thread k ();
            let child = Ctf.note_fork () in
            Ctf.note_switch child;
            fork ~tid:child (fun () ->
                match f () with
                | () ->
                  Ctf.note_resolved child ~ex:None
                | exception ex ->
                  Ctf.note_resolved child ~ex:(Some ex)
              ))
        | Enter fn -> Some (fun k -> fn { Suspended.k; tid })
        | Eio.Private.Effects.Suspend fn ->
          Some (fun k -> 
            let k = { Suspended.k; tid } in
            fn tid (enqueue_result_thread k))
        | _ -> None
    }
  in
  let main_status = ref `Running in
  fork ~tid:(Ctf.mint_id ()) (fun () ->
      match main stdenv with
      | () -> main_status := `Done
      | exception ex -> main_status := `Ex (ex, Printexc.get_raw_backtrace ())
    );
  ignore (Luv.Loop.run () : bool);
  match !main_status with
  | `Done -> ()
  | `Ex (ex, bt) -> Printexc.raise_with_backtrace ex bt
  | `Running -> failwith "Deadlock detected: no events scheduled but main function hasn't returned"
