```ocaml
# #require "eio_posix";;
```

```ocaml
open Eio.Std
```

## Closing an FD removes it from the multiplexer

Closing an FD automatically removes it from epoll's set, meaning that you have
to re-add it using `EPOLL_CTL_ADD`, not `EPOLL_CTL_MOD`.

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run (fun sw ->
     let r, w = Eio_unix.pipe sw in
     Eio_unix.Fd.use_exn "await_writable" (Eio_unix.Resource.fd w) @@ fun fd ->
     Eio_unix.await_writable fd
  );
  (* [r] and [w] are now closed. We'll likely allocate the same FD numbers the second time.
     Check we don't get confused and try to [EPOLL_CTL_MOD] them. *)
  Switch.run (fun sw ->
     let r, w = Eio_unix.pipe sw in
     Eio_unix.Fd.use_exn "await_writable" (Eio_unix.Resource.fd w) @@ fun fd ->
     Eio_unix.await_writable fd
  );;
- : unit = ()
```
