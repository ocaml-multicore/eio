#define CAML_INTERNALS

#include "primitives.h"

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#ifndef _WIN32
static int signal_fd = -1;

static void handle_signal(int signo) {
  int saved_errno = errno;
  ssize_t written;
  caml_record_signal(signo);
  written = write(signal_fd, "!", 1);
  (void) written;
  errno = saved_errno;
}
#endif

CAMLprim value eio_unix_init_signals(value v_unit) {
#ifdef _WIN32
  caml_invalid_argument("signals not supported on Windows");
#else
  int ends[2];

#ifdef __APPLE__
  int r = pipe(ends);
  if (r == -1)
    caml_uerror("pipe2", Nothing);
  for (int i = 0; i < 2; i++) {
    int fd = ends[i];
    r = fcntl(fd, F_GETFD, 0);
    if (r == -1 || fcntl(fd, F_SETFD, r | FD_CLOEXEC) == -1)
      caml_uerror("pipe/cloexec", Nothing);
    r = fcntl(fd, F_GETFL, 0);
    if (r == -1 || fcntl(fd, F_SETFL, r | O_NONBLOCK) == -1)
      caml_uerror("pipe/nonblock", Nothing);
  }
#else
  int r = pipe2(ends, O_CLOEXEC | O_NONBLOCK);
  if (r == -1)
    caml_uerror("pipe2", Nothing);
#endif
  signal_fd = ends[1];
  return Val_int(ends[0]);
#endif
}

/* Call this after Sys.set_signal. It replaces the handler with a similar one
 * that also notifies signal_fd. */
CAMLprim value eio_unix_override_signal_handler(value v_signal_number) {
#ifdef _WIN32
  caml_invalid_argument("signals not supported on Windows");
#else
  struct sigaction sa;
  int signum = caml_convert_signal_number(Int_val(v_signal_number));
  if (signal_fd == -1) {
    caml_invalid_argument("caml_eio_override_signal_handler: must call caml_eio_init_signals first");
  }
  sa.sa_handler = handle_signal;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_ONSTACK;
  if (sigaction(signum, &sa, NULL) == -1) caml_uerror("sigaction", Nothing);
  return Val_unit;
#endif
}
