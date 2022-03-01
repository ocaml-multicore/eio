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
