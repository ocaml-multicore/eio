/* Pseudoterminal support for Eio_unix.Pty. */

#include "primitives.h" /* Defines _GNU_SOURCEfor posix_openpt. */

#include <string.h>
#include <errno.h>

#ifndef _WIN32
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#ifdef _WIN32
CAMLnoret CAMLextern
void caml_unix_error (int errcode, const char * cmdname, value arg);
#endif

/* Returns [pty_fd], opened close-on-exec. */
CAMLprim value eio_unix_open_pty(value v_unit) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "open_pty", Nothing);
#else
  CAMLparam1(v_unit);

#if defined(__OpenBSD__)
  /* OpenBSD posix_openpt accepts only O_RDWR | O_NOCTTY,
     so request close-on-exec separately via fcntl.
     https://man.openbsd.org/posix_openpt.3 */
  int pty_fd = posix_openpt(O_RDWR | O_NOCTTY);
  if (pty_fd < 0)
    caml_uerror("posix_openpt", Nothing);
  if (fcntl(pty_fd, F_SETFD, FD_CLOEXEC) < 0) {
    int e = errno;
    close(pty_fd);
    errno = e;
    caml_uerror("posix_openpt.fcntl", Nothing);
  }
#else
  int pty_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (pty_fd < 0)
    caml_uerror("posix_openpt", Nothing);
#endif  /* __OpenBSD__ */

  CAMLreturn(Val_int(pty_fd));
#endif  /* _WIN32 */
}

/* Returns [(tty_fd, name)], opened close-on-exec. */
CAMLprim value eio_unix_get_pty_peer(value v_pty) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "TIOCGPTPEER", Nothing);
#else
  CAMLparam1(v_pty);
  CAMLlocal2(v_name, v_ret);
  int pty_fd = Int_val(v_pty);

  if (grantpt(pty_fd) < 0)
    caml_uerror("grantpt", Nothing);

  if (unlockpt(pty_fd) < 0)
    caml_uerror("unlockpt", Nothing);

  {
#if defined(__OpenBSD__)
    char *name = ptsname(pty_fd);
    if (name == NULL)
      caml_uerror("ptsname", Nothing);
    v_name = caml_copy_string(name);
#else
    char namebuf[256];
    int err = ptsname_r(pty_fd, namebuf, sizeof namebuf);
    if (err != 0) {
      errno = err;
      caml_uerror("ptsname_r", Nothing);
    }
    v_name = caml_copy_string(namebuf);
#endif
  }

  int tty_fd;
#ifdef TIOCGPTPEER
  tty_fd = ioctl(pty_fd, TIOCGPTPEER, O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (tty_fd < 0)
    caml_uerror("ioctl(TIOCGPTPEER)", Nothing);
#else
  tty_fd = open(String_val(v_name), O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (tty_fd < 0)
    caml_uerror("open", v_name);
#endif

  v_ret = caml_alloc_tuple(2);
  Store_field(v_ret, 0, Val_int(tty_fd));
  Store_field(v_ret, 1, v_name);
  CAMLreturn(v_ret);
#endif
}

CAMLprim value eio_unix_get_winsize(value v_fd) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "get_window_size", Nothing);
#else
  CAMLparam1(v_fd);
  CAMLlocal1(v_ws);
  struct winsize w;
  if (ioctl(Int_val(v_fd), TIOCGWINSZ, &w) < 0)
    caml_uerror("get_window_size", Nothing);
  v_ws = caml_alloc_tuple(4);
  Store_field(v_ws, 0, Val_int(w.ws_row));
  Store_field(v_ws, 1, Val_int(w.ws_col));
  Store_field(v_ws, 2, Val_int(w.ws_xpixel));
  Store_field(v_ws, 3, Val_int(w.ws_ypixel));
  CAMLreturn(v_ws);
#endif
}

CAMLprim value eio_unix_set_winsize(value v_fd, value v_ws) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "set_window_size", Nothing);
#else
  CAMLparam2(v_fd, v_ws);
  struct winsize w;
  memset(&w, 0, sizeof w);
  w.ws_row    = Int_val(Field(v_ws, 0));
  w.ws_col    = Int_val(Field(v_ws, 1));
  w.ws_xpixel = Int_val(Field(v_ws, 2));
  w.ws_ypixel = Int_val(Field(v_ws, 3));
  if (ioctl(Int_val(v_fd), TIOCSWINSZ, &w) < 0)
    caml_uerror("set_window_size", Nothing);
  CAMLreturn(Val_unit);
#endif
}
