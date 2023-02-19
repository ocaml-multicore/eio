#include <sys/stat.h>
#include <sys/types.h>
#include <sys/eventfd.h>
#include <sys/random.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <linux/wait.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

// Make sure we have enough space for at least one entry.
#define DIRENT_BUF_SIZE (PATH_MAX + sizeof(struct dirent64))

CAMLprim value caml_eio_eventfd(value v_initval) {
  int ret;
  ret = eventfd(Int_val(v_initval), EFD_CLOEXEC);
  if (ret == -1) uerror("eventfd", Nothing);
  return Val_int(ret);
}

CAMLprim value caml_eio_mkdirat(value v_fd, value v_path, value v_perm) {
  CAMLparam1(v_path);
  char *path;
  int ret;
  caml_unix_check_path(v_path, "mkdirat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = mkdirat(Int_val(v_fd), path, Int_val(v_perm));
  caml_leave_blocking_section();
  caml_stat_free(path);
  if (ret == -1) uerror("mkdirat", v_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path) {
  CAMLparam2(v_old_path, v_new_path);
  char *old_path;
  char *new_path;
  int ret;
  caml_unix_check_path(v_old_path, "renameat-old");
  caml_unix_check_path(v_new_path, "renameat-new");
  old_path = caml_stat_strdup(String_val(v_old_path));
  new_path = caml_stat_strdup(String_val(v_new_path));
  caml_enter_blocking_section();
  ret = renameat(Int_val(v_old_fd), old_path,
  		 Int_val(v_new_fd), new_path);
  caml_leave_blocking_section();
  caml_stat_free(old_path);
  caml_stat_free(new_path);
  if (ret == -1) uerror("renameat", v_old_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_getrandom(value v_ba, value v_off, value v_len) {
  CAMLparam1(v_ba);
  ssize_t ret;
  ssize_t off = (ssize_t)Long_val(v_off);
  ssize_t len = (ssize_t)Long_val(v_len);
  do {
    void *buf = Caml_ba_data_val(v_ba) + off;
    caml_enter_blocking_section();
    ret = getrandom(buf, len, 0);
    caml_leave_blocking_section();
  } while (ret == -1 && errno == EINTR);
  if (ret == -1) uerror("getrandom", Nothing);
  CAMLreturn(Val_long(ret));
}

CAMLprim value caml_eio_getdents(value v_fd) {
  CAMLparam1(v_fd);
  CAMLlocal2(result, cons);
  char buf[DIRENT_BUF_SIZE];
  struct dirent64 *d;
  int nread, pos;
  caml_enter_blocking_section();
  nread = syscall(SYS_getdents64, Int_val(v_fd), buf, DIRENT_BUF_SIZE);
  caml_leave_blocking_section();
  if (nread == -1) uerror("getdents", Nothing);

  result = Val_int(0); /* The empty list */

  for (pos = 0; pos < nread;) {
    d = (struct dirent64 *) (buf + pos);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, caml_copy_string_of_os(d->d_name)); // Head
    Store_field(cons, 1, result);                            // Tail
    result = cons;
    pos += d->d_reclen;
  }

  CAMLreturn(result);
}

#ifndef __NR_pidfd_open
#define __NR_pidfd_open 434   /* System call # on most architectures */
#endif

static int pidfd_open(pid_t pid, unsigned int flags) {
  return syscall(__NR_pidfd_open, pid, flags);
}

#ifndef __NR_pidfd_send_signal
#define __NR_pidfd_send_signal 424
#endif

static int pidfd_send_signal(int pidfd, int sig, siginfo_t *info, unsigned int flags) {
  return syscall(__NR_pidfd_send_signal, pidfd, sig, info, flags);
}

CAMLprim value caml_eio_pidfd_open(value v_pid) {
  CAMLparam1(v_pid);
  int fd;
  // Returns a file descriptor for the PID with close-on-exec set.
  fd = pidfd_open(Int_val(v_pid), 0);
  if (fd == -1) uerror("pidfd_open", Nothing);
  CAMLreturn(Val_int(fd));
}

CAMLextern int caml_convert_signal_number(int);
CAMLextern int caml_rev_convert_signal_number(int);

CAMLprim value caml_eio_pidfd_send_signal(value v_pidfd, value v_signal) {
  CAMLparam1(v_pidfd);
  int res;

  res = pidfd_send_signal(Int_val(v_pidfd), caml_convert_signal_number(Int_val(v_signal)), NULL, 0);
  if (res == -1) uerror("pidfd_send_signal", Nothing);
  CAMLreturn(Val_unit);
}

// Based on the code in lwt.
#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status)&0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status)&0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status)&0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int status)
{
    value st;

    if (WIFEXITED(status)) {
        st = caml_alloc_small(1, TAG_WEXITED);
        Field(st, 0) = Val_int(WEXITSTATUS(status));
    } else if (WIFSTOPPED(status)) {
        st = caml_alloc_small(1, TAG_WSTOPPED);
        Field(st, 0) =
            Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
    } else {
        st = caml_alloc_small(1, TAG_WSIGNALED);
        Field(st, 0) =
            Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
    }
    return st;
}

CAMLprim value caml_eio_pidfd_wait(value v_pidfd) {
  CAMLparam1(v_pidfd);
  CAMLlocal1(status);
  int res;
  siginfo_t info;

  res = waitid(P_PIDFD, Int_val(v_pidfd), &info, WEXITED);
  if (res == -1) uerror("pidfd_wait", Nothing);
  status = alloc_process_status(info.si_status);
  CAMLreturn(status);
}