```ocaml
# #require "eio_posix";;
```

```ocaml
open Eio.Std
```

## Closing an FD removes it from epoll

```ocaml
# Eio_posix.run @@ fun _env ->
  Switch.run (fun sw ->
     let r, w = Eio_unix.pipe sw in
     Eio_unix.await_writable (Eio_unix.FD.peek w)
  );
  Switch.run (fun sw ->
     let r, w = Eio_unix.pipe sw in
     Eio_unix.await_writable (Eio_unix.FD.peek w)
  );;
- : unit = ()
```
