## v0.4

Note: Eio 0.4 drops compatibility with OCaml 4.12+domains. Use OCaml 5.0.0~alpha1 instead.

API changes:

- `Eio.Dir` has gone. Use `Eio.Path` instead (@talex5 #266 #270).

- `Eio_unix.FD.{take,peek}` were renamed to `take_opt`/`peek_opt` to make way for non-optional versions.

New features:

- Fiber-local storage (@SquidDev #256).  
  Attach key/value bindings to fibers. These are inherited across forks.

- `Eio.Path.{unlink,rmdir,rename}` (@talex5 #264 #265).

- `Eio_main.run` can now return a value (@talex5 #263).  
  This is useful for e.g. cmdliner.

- `Eio_unix.socketpair` (@talex5 #260).

- `Fiber.fork_daemon` (@talex5 #252).  
  Create a helper fiber that does not prevent the switch from exiting.

- Add `Fiber.{iter,map,filter,fiter_map}` (@talex5 #248 #250).  
  These are concurrent versions of the corresponding operations in `List`.

Bug fixes:

- Fix scheduling fairness in luv backend (@talex5 #269).

- Implement remaining shutdown commands for luv (@talex5 #268).

- Fix IPv6 support with uring backend (@haesbaert #261 #262).

- Use `Eio.Net.Connection_reset` exception in more places (@talex5 #257).

- Report use of closed FDs better (@talex5 #255).  
  Using a closed FD could previously cause the whole event loop to exit.

- Some fixes for cancellation (@talex5 #254).

- Ensure `Buf_write` still flushes if an exception is raised (@talex5 #246).

- Do not allow close on `accept_fork` socket (@talex5 #245).

Documentation:

- Document integrations with Unix, Lwt and Async (@talex5 #247).

- Add a Dockerfile for easy testing (@talex5 #224).  

## v0.3

API changes:

- `Net.accept_sub` is deprecated in favour of `accept_fork` (@talex5 #240).  
  `Fiber.fork_on_accept`, which it used internally, has been removed.

- Allow short writes in `Read_source_buffer` (@talex5 #239).  
  The reader is no longer required to consume all the data in one go.
  Also, add `Linux_eio.Low_level.writev_single` to expose this behaviour directly.

- `Eio.Unix_perm` is now `Eio.Dir.Unix_perm`.

New features:

- Add `Eio.Mutex` (@TheLortex @talex5 #223).

- Add `Eio.Buf_write` (@talex5 #235).  
  This is a buffered writer for Eio sinks, based on Faraday.

- Add `Eio_mock` library for testing (@talex5 #228).  
  At the moment it has mock flows and networks.

- Add `Eio_mock.Backend` (@talex5 #237 #238).  
  Allows running tests without needing a dependency on eio_main.
  Also, as it is single-threaded, it can detect deadlocks in test code instead of just hanging.

- Add `Buf_read.{of_buffer, of_string, parse_string{,_exn}, return}` (@talex5 #225).

- Add `<*>` combinator to `Buf_read.Syntax` (@talex5 #227).

- Add `Eio.Dir.read_dir` (@patricoferris @talex5 #207 #218 #219)

Performance:

- Add `Buf_read` benchmark and optimise it a bit (@talex5 #230).

- Inline `Buf_read.consume` to improve performance (@talex5 #232).

Bug fixes / minor changes:

- Allow IO to happen even if a fiber keeps yielding (@TheLortex @talex5 #213).

- Fallback for `traceln` without an effect handler (@talex5 #226).  
  `traceln` now works outside of an event loop too.

- Check for cancellation when creating a non-protected child context (@talex5 #222).

- eio_linux: handle EINTR when calling `getrandom` (@bikallem #212).

- Update to cmdliner.1.1.0 (@talex5 #190).

## v0.2

- Add support for UDP (@patricoferris #171).

- Rename Fibre to Fiber (@talex5 #195). This is to match the compiler's spelling.

- Switch to luv backend if uring can't be used (@talex5 #203).
  Useful on Windows with WSL, and also in Docker containers on older systems.

- Eio_linux: cope with lack of fixed chunks (@talex5 #200).
  - If we run out of fixed memory, just use regular memory instead of waiting (which might deadlock).
  - If we try to allocate a fixed buffer and fail, we now just log a warning and continue without one.

- Add support for FD passing with Eio_linux (@talex5 #199).

- Add `Eio_unix.FD.as_socket` (@talex5 #193).
  Useful for working with existing libraries that provide a `Unix.file_descr`, or for receiving FDs from elsewhere (e.g. socket activation).
  Also, the `Luv.{File,Handle}.of_luv` functions now allow controlling whether to close the wrapped FD.

- Add `Eio_unix.sleep` (@talex5 #188). Based on feedback that some people don't want to treat time as a capability. Possibly also useful for debugging race conditions.

- Tidy up forking API (@talex5 #192). Moves some common code out the the individual backends.

- Improve documentation (@talex5 #197 #194 #186 #185). In particular, explain more low-level details about how cancellation works.

- Add an example `Eio_null` backend (@talex5 #189). This supports creating fibers, promises and cancellation, but provides no IO operations.

- `Effect.eff` is now `Effect.t` in OCaml trunk (@talex5 #201).

## v0.1

- Initial release.
