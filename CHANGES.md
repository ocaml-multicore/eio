New features / API changes:

- Add `Eio.Process` for cross-platform subprocess support (@patricoferris @talex5 #499, reviewed by @anmonteiro @avsm @haesbaert).

- Add `Eio_unix.Net module` (@talex5 #516, reviewed by @avsm).  
  The Unix network APIs have been cleaned up and moved here, and some missing datagram operations have been added.
  `send` now takes an iovec, not just a single buffer.

- Add support for domain local await (@polytypic @talex5 #494 #503).  
  Allows sharing e.g. kcas data-structures across Eio and Domainslib domains.

- Add initial eio_windows backend (@patricoferris @talex5 #497 #530 #511 #523 #509, reviewed by @avsm @polytypic).  

- Remove eio_luv backend (@talex5 #485).  
  It was only used on Windows, and has been replaced by eio_windows.

- Unify `Eio_linux.FD` and `Eio_posix.Fd` as `Eio_unix.Fd` (@talex5 #491).  
  Now that eio_luv is gone, there is no need for different backends to have different types for wrapped file descriptors.

- Move `Eio.Stdenv.t` to `Eio_unix.Stdenv.base` (@talex5 #498).  
  Note that the rest of `Eio.Stdenv` is still there; only the definition of a full Unix-like environment has moved.

- Deprecation cleanups (@talex5 #508).  
  Removed some APIs that were already marked as deprecated in Eio 0.8.

Bug fixes:

- eio_linux: fall back to `fork` if `clone3` is unavailable (@talex5 #524, reported by @smondet, reviewed by @avsm).  
  Docker's default security policy blocks `clone3`.

- Don't call `accept_fork`'s error handler on cancellation (@talex5 #520).  
  This isn't an error and should not be reported.

- Fix `eio_unix_is_blocking` C stub (@patricoferris #505, reviewed by @talex5).

- Fix `Condition.await bug` when cancelling (@polytypic @talex5 #487).

- Buf_write: fix flush returning too early (@talex5 #539, reported by @cometkim).

- Ignore `ENOTCONN` errors on socket shutdown (@avsm #533, reported by @patricoferris, reviewed by @talex5).

Documentation:

- Link to developer meetings information (@talex5 @Sudha247 #515).

- Adopt OCaml Code of Conduct (@Sudha247 #501).

- Add README links to Meio and Lambda Capabilities blog post (@talex5 #496).

- Document mirage `Ipaddr` conversion (@RyanGibb @patricoferris @talex5 #492).

- Document how to use Domainslib from Eio (@talex5 #489, reviewed by @polytypic @patricoferris).

Other changes:

- Run benchmarks with current-bench (@Sudha247 @talex5 #500).

- Fix MDX tests on OCaml 5.1 (@talex5 #526).

- Add stress test for spawning processes (@talex5 #519).  
  This was an attempt to track down the https://github.com/ocaml/ocaml/issues/12253 signals bug.

- `Eio.Process.pp_status` should be polymorphic (@talex5 #518).

- eio_posix: probe for existence of some flags (@talex5 #507, reported by @hannesm).  
  FreeBSD 12 didn't have `O_DSYNC`. Also, add `O_RESOLVE_BENEATH` and `O_PATH` if available.

- Fix race in ctf tests (@talex5 #493).

## v0.9

New features:

- Add eio_posix backend (@talex5 @haesbaert #448 #477, reviewed by @avsm @patricoferris @polytypic).  
  This replaces eio_luv on all platforms except Windows (which will later switch to its own backend). It is a lot faster, provides access to more modern features (such as `openat`), and can safely share OS resources between domains.

- Add subprocess support (@patricoferris @talex5 #461 #464 #472, reviewed by @haesbaert @avsm).  
  This is the low-level API support for eio_linux and eio_posix. A high-level cross-platform API will be added in the next release.

- Add `Fiber.fork_seq` (@talex5 #460, reviewed by @avsm).  
  This is a light-weight alternative to using a single-producer, single-consumer, 0-capacity stream, similar to a Python generator function.

Bug fixes:

- eio_linux: make it safe to share FDs across domains (@talex5 #440, reviewed by @haesbaert).  
  It was previously not safe to share file descriptors between domains because if one domain used an FD just as another was closing it, and the FD got reused, then the original operation could act on the wrong file.

- eio_linux: release uring if Linux is too old (@talex5 #476).  
  Avoids a small resource leak.

- eio_linux: improve error handling creating pipes and sockets (@talex5 #474, spotted by @avsm).  
  If we get an error (e.g. too many FDs) then report it to the calling fiber, instead of exiting the event loop.

- eio_linux: wait for uring to finish before exiting (@talex5 #470, reviewed by @avsm).  
  If the main fiber raised an exception then it was possible to exit while a cancellation operation was still in progress.

- eio_main: make `EIO_BACKEND` handling more uniform (@talex5 #447).  
  Previously this environment variable was only used on Linux. Now all platforms check it.

- Tell dune about `EIO_BACKEND` (@talex5 #442).  
  If this changes, dune needs to re-run the tests.

- eio_linux: add some missing close-on-execs (@talex5 #441).

- eio_linux: `read_exactly` fails to update file offset (@talex5 #438).

- Work around dune `enabled_if` bug on non-Linux systems (@polytypic #475, reviewed by @talex5).

- Use raw system call of `getrandom` for glibc versions before 2.25 (@zenfey #482).

Documentation:

- Add `HACKING.md` with hints for working on Eio (@talex5 #443, reviewed by @avsm @polytypic).

- Improve worker pool example (@talex5 #454).

- Add more Conditions documentation (@talex5 #436, reviewed by @haesbaert).  
  This adds a discussion of conditions to the README and provides examples using them to handle signals.

- Condition: fix the example in the docstring (@avsm #468).

Performance:

- Add a network benchmark using an HTTP-like protocol (@talex5 #478, reviewed by @avsm @patricoferris).

- Add a benchmark for reading from `/dev/zero` (@talex5 #439).

Other changes:

- Add CI for macOS (@talex5 #452).

- Add tests for `pread`, `pwrite` and `readdir` (@talex5 #451).

- eio_linux: split into multiple files (@talex5 #465 #466, reviewed by @avsm).

- Update Dockerfile (@talex5 #471).

- Use dune.3.7.0 (@patricoferris #457).

- Mint exclusive IDs across domains (@TheLortex #480, reported by @haesbaert, reviewed by @talex5).  
  The tracing currently only works with a single domain anyway, but this will change when OCaml 5.1 is released.


## v0.8.1

Some build fixes:

- Fix build on various architectures (@talex5 #432).
  - Work around dune `%{system}` bug.
  - eio_luv: fix `max_luv_buffer_size` on 32-bit platforms.

- Add missing test-dependency on MDX (@talex5 #430).

## v0.8

New features:

- Add `Eio.Net.run_server` (@bikallem @talex5 #408).  
  Runs an accept loop in one or more domains, with cancellation and graceful shutdown,
  and an optional maximum number of concurrent connections.

- Add `Buf_read.BE` and `LE` parsers (@Cjen1 #399).  
  Parse numbers in various binary formats.

- Add `Eio.Buf_read.uint8` (@talex5 #418).

Performance:

- Make `Eio.Condition` lock-free (@talex5 #397 #381).  
  In addition to being faster, this allows using conditions in signal handlers.

- Make `Eio.Semaphore` lock-free (@talex5 @polytypic #398).

- Make `Eio.Stream` lock-free when the capacity is zero (@talex5 #413 #411).

- Make `Eio.Promise` lock-free (@talex5 #401).

Bug fixes:

- eio_linux: call `Uring.submit` as needed (@talex5 @bikallem #428).  
  Previously, we could fail to submit a job promptly because the SQE queue was full.

- Fix luv signals (@haesbaert #412).  
  `libuv` automatically retries polling if it gets `EINTR`, without giving OCaml signal handlers a chance to run.

- eio_luv: fix some resource leaks (@talex5 @patricoferris #421).

- eio_luv: fix "unavailable signal" error on Windows (@talex5 #420, reported by @nojb).

- Fix `Buf_write.BE.uint48` and `LE.uint48` (@adatario #418).

Documentation:

- Add example programs (@talex5 #389).

- Update network examples to use `run_server` (@talex5 #417).

- Add a warning to the tutorial about `Fiber.first` (@talex5 #394).

- Clarify the epoch used for `Eio.Time.now` (@bikallem #395).

- Describe `secure_random` as an infinite source (@patricoferris #426).

- Update README for OCaml 5 release (@talex5 #384 #391 #393).

Other changes:

- Delay setting `SIGPIPE` handler until the `run` function is called (@talex5 #420).

- Remove debug-level logging (@talex5 #403).

- eio-luv: improve `process.md` test (@smondet #414).

- Update to Dune 3 (@talex5 #410).

- Remove test dependency on Astring (@talex5 #402 #404).

- Simplify cancellation logic (@talex5 #396).

- time: `Mtime.Spand.to_s` has been deprecated in mtime 2.0.0 (@bikallem #385).

## v0.7

API changes:

- Unify IO errors as `Eio.Io` (@talex5 #378).  
  This makes it easy to catch and log all IO errors if desired.
  The exception payload gives the type and can be used for matching specific errors.
  It also allows attaching extra information to exceptions, and various functions were updated to do this.

- Add `Time.Mono` for monotonic clocks (@bikallem @talex5 #338).  
  Using the system clock for timeouts, etc can fail if the system time is changed during the wait.

- Allow datagram sockets to be created without a source address (@bikallem @haesbaert #360).  
  The kernel will allocate an address in this case.
  You can also now control the `reuse_addr` and `reuse_port` options.

- Add `File.stat` and improve `Path.load` (@haesbaert @talex5 #339).  
  `Path.load` now uses the file size as the initial buffer size.

- Add `Eio_unix.pipe` (@patricoferris #350).  
  This replaces `Eio_linux.pipe`.

- Avoid short reads from `getrandom(2)` (@haesbaert #344).  
  Guards against buggy user code that might not handle this correctly.

- Rename `Flow.read` to `Flow.single_read` (@talex5 #353).  
  This is a low-level function and it is easy to use it incorrectly by ignoring the possibility of short reads.

Bug fixes:

- Eio_luv: Fix non-tail-recursive continue (@talex5 #378).  
  Affects the `Socket_of_fd` and `Socketpair` effects.

- Eio_linux: UDP sockets were not created close-on-exec (@talex5 #360).

- Eio_linux: work around io_uring non-blocking bug (@haesbaert #327 #355).  
  The proper fix should be in Linux 6.1.

- `Eio_mock.Backend`: preserve backtraces from `main` (@talex5 #349).

- Don't lose backtrace in `Switch.run_internal` (@talex5 #369).

Documentation:

- Use a proper HTTP response in the README example (@talex5 #377).

- Document that read_dir excludes "." and ".." (@talex5 #379).

- Warn about both operations succeeding in `Fiber.first` (@talex5 #358, reported by @iitalics).

- Update README for OCaml 5.0.0~beta2 (@talex5 #375).

Backend-specific changes:

- Eio_luv: add low-level process support (@patricoferris #359).  
  A future release will add Eio_linux support and a cross-platform API for this.

- Expose `Eio_luv.Low_level.Stream.write` (@patricoferris #359).

- Expose `Eio_luv.Low_level.get_loop` (@talex5 #371).  
  This is needed if you want to create resources directly and then use them with Eio_luv.

- `Eio_linux.Low_level.openfile` is gone (@talex5 #378).  
  It was just left-over test code.


## v0.6

Changes:

- Update to OCaml 5.0.0~beta1 (@anmonteiro @talex5 #346).

- Add API for seekable read/writes (@nojb #307).

- Add `Flow.write` (@haesbaert #318).  
  This provides an optimised alternative to `copy` in the case where you are writing from a buffer.

- Add `Net.with_tcp_connect` (@bikallem #302).  
  Convenience function for opening a TCP connection.

- Add `Eio.Time.Timeout` (@talex5 #320).  
  Makes it easier to pass timeouts around.

- Add `Eio_mock.Clock` (@talex5 #328).  
  Control time in tests.

- Add `Buf_read.take_while1` and `skip_while1` (@bikallem #309).  
  These fail if no characters match.

- Make the type parameter for `Promise.t` covariant (@anmonteiro #300).

- Move list functions into a dedicated submodule (@raphael-proust #315).

- Direct implementation of `Flow.source_string` (@c-cube #317).  
  Slightly faster.

Bug fixes:

- `Condition.broadcast` must interlock as well (@haesbaert #324).

- Split the reads into no more than 2^32-1 for luv (@haesbaert @talex5 @EduardoRFS #343).  
  Luv uses a 32 bit int for buffer sizes and wraps if the value passed is too big.

- eio_luv: allow `Net.connect` to be cancelled (@talex5 @nojb #311).

- eio_main: Use dedicated log source (@anmonteiro #326).

- linux_eio: fix kernel version number in log message (@talex5 @nojb #314).

- Account for stack differences in the socketpair test (issue #312) (@haesbaert #313).

Documentation:

- Add Best Practices section to README (@talex5 #299).

- Documentation improvements (@talex5 #295 #337).


## v0.5

New features:

- Add `Eio.Condition` (@TheLortex @talex5 #277).  
  Allows a fiber to wait for some condition to become true.

- Add `Eio.Net.getaddrinfo` and `getnameinfo` (@bikallem @talex5 #278 #288 #291).  
  Convert between host names and addresses.

- Add `Eio.Debug` (@talex5 #276).  
  Currently, this allows overriding the `traceln` function.

- `Buf_write.create`: make switch optional (@talex5 #283).  
  This makes things easier for people porting code from Faraday.

Bug fixes:

- Allow sharing of libuv poll handles (@patricoferris @talex5 #279).  
  Luv doesn't allow two callers to watch the same file handle, so we need to handle that in Eio.

Other changes:

- Upgrade to uring 0.4 (@talex5 #290).

- Mention `Mutex`, `Semaphore` and `Condition` in the README (@talex5 #281).

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
