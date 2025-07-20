## v1.3

Bug fixes:

- posix: ensure `spawn_unix` wraps errors when calling `openat` (@dijkstracula #809).

- Use `O_RESOLVE_BENEATH` on FreeBSD (@talex5 #810, reported by @dijkstracula).  
  FreeBSD needs `-D__BSD_VISIBLE` to be able to see this.
  Fixed the CI bug this revealed, which had started also affecting macos.

- Ignore `ECONNRESET` on close (@talex5 #787).  
  FreeBSD returns `ECONNRESET` in certain (unclear) circumstances, but it does still close the FD successfully.
  If you care about this case, you should probably use `shutdown` instead and check for it there.
  Python and Ruby at least both explicitly check for and ignore this error too.

- On Windows, fix stdin broken-pipe and blocked domains (@bdodrem @talex5 #795).
  - Ensure blocking FDs are ready before trying to use them.
  - Replace `eio_windows_cstruct_stubs.c` by Unix functions added in OCaml 5.2,
    which correctly handle Window's strange use of `EPIPE`.

Documentation:

- Documentation fixes (@jonludlam #813).

- Minor documentation improvements (@talex5 #794).

Build fixes:

- Disable `dune subst` (@talex5 #789).

## v1.2

Changes:

- Make `fork_action.h` a public header (@patricoferris #769, reviewed by @talex5).  
  Allows other libraries to add new actions.

- Record trace event when spawning processes (@talex5 #749).  
  Spawning a subprocess can take a long time in some cases, so show it clearly in the traces.

- Eio_unix.Net: make some return types more polymorphic (@talex5 #744).

Bug fixes:

- Preserve backtraces in `fork_daemon` (@talex5 #779).

- Eio.Path: always use "/" as separator (@talex5 #743).

Linux backend:

- Allow `alloc_fixed_or_wait` to be cancelled (@talex5 #753).

- Avoid triggering a (harmless) TSan warning (@talex5 #754, reported by @avsm).

Windows backend:

- Unregister FDs on cancel (@talex5 #756).  
  Fixes `exception Unix.Unix_error(Unix.ENOTSOCK, "select", "")`.

- Work around problems in `Unix.getaddrinfo` (@talex5 #780).  
  Fixes e.g. `No addresses found for host name "127.0.0.1"`.

- Group `ECONNABORTED` with other connection reset errors (@talex5 #783).

- Check `has_symlink` for tests (@create2000 #771, reviewed by @patricoferris and @talex5).

- Improve `openat` error handling (@talex5 #742, reported by @kentookura).  
  Fixes `exception Unix.Unix_error(Unix.ENOENT, "openat", "")`.

Documentation:

- examples/fs: show how to read files while scanning (@talex5 #745).

- Add example to `Buf_read.seq` documentation (@talex5 #739, requested by @darrenldl and @rizo).

Build and test:

- Fix tests on OpenBSD (@talex5 #782).

- Add advice about using AI for code generation (@patricoferris #765, reviewed by @avsm and @talex5).

- Minor code cleanups (@talex5 #755).

- Define `struct clone_args` for linux-lts versions that don't have it (@copy #741, reviewed by @talex5).

- eio_linux: refactor fixed buffer code (@talex5 #752).

## v1.1

New features:

- Add `Eio.Path.symlink` (@patricoferris #715, reviewed by @talex5).

- Add `Eio.Pool.use ~never_block` (@SGrondin #657, reviewed by @talex5).

- Add `Eio_unix.Net.import_socket_listening` (@alyssais #733).

- Add `Eio.Time.Timeout.sleep` (@talex5 #726).

Documentation:

- Add `examples/fs` showing how to walk a directory tree (@talex5 #730).

- README: explain that `read_all` reads until shutdown (@talex5 #717, reported by @Wenke-D).

- Use long dash in README title (@lucperkins #718).

Linux backend:

- Require Linux >= 5.15 (@talex5 #720, reviewed by @SGrondin and @avsm).  
  Removes a work-around that required checking whether every flow was a tty.

- Don't call submit immediately before wait (@talex5 #728).  
  This is slightly faster and makes the traces clearer.

- Don't record submit events when there's nothing to submit (@talex5 #729).  
  Makes the traces a bit clearer.

- Split flow into its own file (@talex5 #727).

- Add work-around for signals race (@talex5 #734).

POSIX backend:

- Add `_BSD_SOURCE` flag to fix build on OpenBSD (@prgbln #722).

- Fix sandboxed path resolution on OpenBSD (@jebrosen #723, reviewed by @talex5).  
  OpenBSD uses `ELOOP` when opening a symlink with `O_NOFOLLOW`.

Build and test:

- Benchmarks: record uname, Eio backend, and number of cores (@talex5 #719).

- Update to MDX 2.4.1 for OCaml 5.2 (@talex5 #712).

## v1.0

New features:

- Add `Eio_unix.Cap` module to enable Capsicum mode (@talex5 #697, reviewed by @SGrondin).

- eio_linux: expose more functions in the `Low_level` module (@talex5 #705, reviewed by @SGrondin).  
  Add all the functions used by other parts of eio_linux (`openat`, `mkdir`, `read_link`, `unlink`, `rename` and `pipe`).
  Tidied the API up a bit too:
  - `mkdir_beneath` is now just `mkdir`.
  - `statx_confined` is now just `statx`.
  - `open_dir` is gone; the single user now calls `openat` directly.

Documentation:

- Add README documentation for `Eio.Executor_pool` (@SGrondin @talex5 #707, reviewed by @Sudha247).

- eio_linux: remove logging (@talex5 #708, requested by @clecat).  
  There were only two remaining uses of Logs, neither of which has proved useful.

Build:

- Add upper-bound on MDX (@talex5 #706).  
  The new version attempts to execute included blocks.

- Fix tests to pass with both old and new Kcas (@polytypic #704).

- Make posix `open_beneath` test idempotent (@SGrondin #703).

- Executor_pool: mention requested weight in error message (@talex5 #702, reported by @yawaramin).

## v0.15

New features:

- eio_posix: use directory FDs instead of realpath (@talex5 #694 #696, reviewed by @SGrondin).  
  Using realpath was an old hack from the libuv days and subject to races. It was also slow.

- Keep pool of systhreads for blocking operations (@SGrondin @talex5 #681).  
  This is much faster than creating a new thread for each operation.
  It mainly benefits the eio_posix backend, as that uses lots of systhreads.

- Make `Switch.on_release` thread-safe (@talex5 #684, requested by @art-w and @clecat).  
  This allows resource pools to be shared between domains easily.

- Add `Eio.Path.read_link` (@talex5 #686).

- Add `Eio_unix.Fd.is_open` (@talex5 #690).

- Include backtrace in systhread errors (@talex5 #688, reviewed by @SGrondin).  
  Also, add `Eio.Exn.empty_backtrace` as a convenience.

- eio.mock: add tracing support to mock backend (@talex5 #687).

- Improve tracing (@talex5 #675 #683 #676, reviewed by @SGrondin).  
  Update tracing section of README and trace more things
  (`run_in_systhread`, `close`, `submit`, `traceln`, cancellation and domain spawning).

Documentation:

- Link to verification work in docs (@talex5 #682).

- Add more trace diagrams to README (@talex5 #698).

- Adjust COC contacts (@polytypic #685, reviewed by @Sudha247).

Bug fixes:

- eio_linux: retry `openat2` on `EAGAIN` (@talex5 #693, reviewed by @SGrondin).

- eio_posix and eio_windows: check for IO periodically (@talex5 #674).

- Handle EPERM when trying to initialise uring (@talex5 #691).  
  This can happen when using a Docker container.

Build and tests:

- Benchmark `Eio_unix.run_in_systhread` (@talex5 #678, reviewed by @SGrondin).

- Enable lintcstubs for `Eio_unix.Private` too (@talex5 #689).

- Stat benchmark: report cleanup time and optimise (@talex5 #692).

- Make benchmarks start faster (@talex5 #673).

- Update build for new eio-trace CLI (@talex5 #699).

- Expect opam-repo-ci tests to fail on macos (@talex5 #672).

## v0.14

New features / API changes:

- Add `Eio.Executor_pool` (@SGrondin #639, reviewed by @talex5).  
  Provides an easy way to distribute jobs across domains.

- Add `Fiber.first ~combine` and `Fiber.n_any` (@SGrondin @talex5 #587).  
  Allows keeping both results in the case where multiple fibers succeed.

- Add `Eio_mock.Backend.run_full` with auto-advancing mock clock (@talex5 #644, reviewed by @SGrondin).  
  Simplifies testing of code using clocks.

- Add `Buf_write.printf` (@SGrondin @talex5 #655).

- Add `Net.listening_addr` (@mefyl #555, reviewed by @patricoferris @talex5).  
  Useful to get the socket's address if the OS assigns it.

- Add `Promise.try_resolve` (@talex5 #646).

- Remove `Cancel_hook_failed` exception (@talex5 #640).  
  Didn't seem to be used and broke dscheck.

Tracing:

- Improve tracing (@TheLortex @patricoferris @talex5 #656).  
  Trace cancellation contexts and OS operations, and simplify API.

- Add labels to switches (@talex5 #661, reviewed by @SGrondin).

- `Fiber.all`: use the parent fiber (@talex5 #665, reviewed by @SGrondin).  
  Cleans up the traces a bit.

Performance:

- Faster and simpler `Lf_queue` (@talex5 #647, based on work by @polytypic).

- Optimise `Flow.copy` with `Buf_read.as_flow` (@talex5 #663, reviewed by @SGrondin, reported by @leostera).

Bug fixes:

- Fix handling of very long IO vectors (@talex5 #653, reported by @Cjen1).

- eio_posix: use `caml_enter_blocking_section` in more places (@talex5 #654, reviewed by @SGrondin).

- eio_posix: work around `caml_unix_alloc_sockaddr` bug (@talex5 #651).

- Remove default backtrace from `Switch.fail` (@talex5 #664).

Documentation:

- Organise eio.mli better (@talex5 #667).

- Fix quoting of quotes in process error messages (@talex5 #666, reviewed by @SGrondin).

- Mention Path module in File and Fs documentation (@talex5 #659, requested by @clecat).

- Minor documentation updates (@SGrondin @talex5 #670).

Build / internals:

- Allow closing synchronous streams (@talex5 #641, reviewed by @SGrondin).  
  This isn't currently exposed in the public interface.

- Fix non-idempotent tests (@SGrondin #662).

- eio_windows: add explicit fmt dependency (@talex5 #643).

## v0.13

New features / API changes:

- Add `Flow.read_all` (@SGrondin #596, reviewed by @talex5 @rbjorklin).

- Add `Path.stat` (@patricoferris @talex5 @avsm #617 #618 #624 #620, reviewed by @SGrondin).

- Add `Path.rmtree` (@talex5 #627 #628, reviewed by @SGrondin).

- Add `Path.mkdirs` and `Path.split` (@patricoferris @talex5 #625).

- Add `Eio.File.{seek,sync,truncate}` (@talex5 #626).

- Add `Eio.Path.{kind,is_file,is_directory}` (@patricoferris @talex5 #623, reviewed by @avsm).

- Switch from CTF to OCaml 5.1 runtime events (@TheLortex @patricoferris @talex5 #634 #635, reviewed by @avsm).
  This is a minimal initial version.

Documentation:

- Document `File.Stat` record fields (@avsm @talex5 #621).

- Update README section about `env` (@talex5 #614, reported by @jonsterling).

Build and test changes:

- Add `File.stat` benchmark (@talex5 #616).

- Add `Path.stat` benchmark (@patricoferris @talex5 #630).

- eio_linux: mark as only available on Linux (@talex5 #629).

- Make MDX tests idempotent (@SGrondin #601, reviewed by @talex5).

- Allow trailing whitespace in CHANGES.md (@talex5 #632).

- Update minimum OCaml version to 5.1 (@talex5 #631).

- Generate prototypes for C stubs from ml files (@talex5 #615).

- Don't try to compile uring support on centos 7 (@talex5 #638, reported by @zenfey).

## v0.12

New features / API changes:

- Replace objects with variants (@talex5 @patricoferris #553 #605 #608, reviewed by @avsm).  
  Some potential users found object types confusing, so we now use an alternative scheme for OS resources.
  For users of the resources, the only thing that changes is the types:

  - Instead of taking an argument of type `#foo`, you should now take `_ foo`.
  - Instead of returning a value of type `foo`, you should now return `foo_ty Eio.Resource.t`.

  To provide your own implementation of an interface, you now provide a module rather than an object.
  For example, to provide your own source flow, use `Eio.Flow.Pi.source (module My_source)`.

  If you want to define your own interfaces, see the `Eio.Resource` module documentation.

- Add `Eio.Pool` (@talex5 @darrenldl #602, reviewed by @patricoferris).  
  A lock-free pool of resources. This is similar to `Lwt_pool`.

- Add `Eio.Lazy` (@talex5 #609, reviewed by @SGrondin).  
  If one fiber tries to force a lazy value while another is already doing it,
  this will wait for the first one to finish rather than raising an exception (as `Stdlib.Lazy` does).

- Add `Eio.Path.native` (@talex5 #603, reviewed by @patricoferris).  
  This is useful when interacting with non-Eio libraries, for spawning sub-processes, and for displaying paths to users.

- Add `Flow.single_write` (@talex5 #598).

- Add `Eio.Flow.Pi.simple_copy` (@talex5 #611).  
  Provides an easy way to implement the `copy` operation when making your own sink.

- Eio_unix: add FD passing (@talex5 #522).  
  Allows opening a file and passing the handle over a Unix-domain socket.

- Add `Process.run ?is_success` to control definition of success (@SGrondin #586, reviewed by @talex5).

- Add `Eio_mock.Domain_manager` (@talex5 #610).  
  This mock domain manager runs everything in a single domain, allowing tests to remain deterministic.

- Add `Eio.Debug.with_trace_prefix` (@talex5 #610).
  Allows prefixing all `traceln` output. The mock domain manager uses this to indicate which fake domain is running.

Bug fixes:

- Fork actions must not allocate (@talex5 #593).  
  When using multiple domains, child processes could get stuck if they forked while another domain held the malloc lock.

- eio_posix: ignore some errors writing to the wake-up pipe (@talex5 #600).  
  If the pipe is full or closed, the wake-up should simply be ignored.

Build/test fixes:

- Fix some MDX problems on Windows (@polytypic #597).

- The README depends on kcas (@talex5 #606).

- Clarify configuration for lib_eio_linux and enable tests on other arches (@dra27 #592).

- eio_linux tests: skip fixed buffer test if not available (@talex5 #604).

- eio_windows: update available line to win32 (@talex5 #588 #591).


## v0.11

New features / API changes:

- Extend `Eio.Condition` API (@talex5 #563).  
  - `loop_no_mutex` is a simpler and more efficient way to way for a condition.
  - `register_immediate` allows integration with other IO libraries.

- Expose `Eio.Stdenv.backend_id` (@bord-o #560, reviewed by @talex5).  
  Useful in tests to report which backend is being used.

- Remove deprecated features (@talex5 #552, reviewed by @avsm).  
  These were all already marked as deprecated in v0.10 and are now gone completely:
  - `Fiber.fork_sub`
  - `Eio_unix.{FD,Ipaddr,socketpair,getnameinfo}`
  - `Eio_linux.{FD,get_fd,get_fd_opt}`
  - `Eio_posix.Low_level.Fd`

- Allow calling `close` more than once (@talex5 #547, requested by @anmonteiro, reviewed by @patricoferris, @avsm).

- Add `close` to socket type (@talex5 #549).  
  Simplifies the type signatures a bit by avoiding having to mention this everywhere.

Bug fixes:

- Fix handling of empty path strings (@talex5 #569, reported by @SGrondin).  
  Using "" instead of "." in some places resulted in an error.

- eio_posix: fix update to watched FDs on cancel (@talex5 #574, reported and reviewed by @quernd).  
  Cancelling the last watcher of an FD didn't remove it from the set passed to `poll`,
  which could result in constant wake-ups.

- eio_posix: fix `pread` at end-of-file (@talex5 #581, reported by @SGrondin).  
  It tried to return 0 instead of `End_of_file`, triggering an assertion.

- eio_posix: don't reap non-Eio child processes (@talex5 #562).  
  This allows spawning processes with e.g. the stdlib or Lwt
  (but see https://github.com/ocaml-multicore/lwt_eio/pull/19 for Lwt support).

- Preserve backtraces across `Domain_manager.run` (@talex5 #571).  
  See https://github.com/ocaml/ocaml/issues/12362.

- Correct the backend selection for Cygwin (@dra27 #557).  
  Use `eio_posix`, not `eio_windows` in this case.

Other changes:

- Simplify dune files with dune 3.9's `build_if` (@talex5 #582).

- Remove `Waiters` from `Eio_core` (@talex5 #567).  
  `Eio.Switch` no longer uses this so it can finally be removed.

- Use `Fmt.Dump.signal` to format signals (@talex5, @MisterDA #543).

Documentation:

- Add some notes about thread-safety in the documentation (@talex5 #568).

## v0.10

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
