#define _GNU_SOURCE
#include <linux/sched.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/eventfd.h>
#if __GLIBC__ > 2 || __GLIBC_MINOR__ > 24
#include <sys/random.h>
#endif
#include <sys/syscall.h>
#include <sys/wait.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

// We need caml_convert_signal_number
#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#include "fork_action.h"

#ifndef SYS_pidfd_send_signal
# define SYS_pidfd_send_signal 424
#endif
#ifndef SYS_pidfd_open
# define SYS_pidfd_open 434
#endif
#ifndef SYS_clone3
# define SYS_clone3 435
# define CLONE_PIDFD 0x00001000
struct clone_args {
  uint64_t flags;
  uint64_t pidfd;
  uint64_t child_tid;
  uint64_t parent_tid;
  uint64_t exit_signal;
  uint64_t stack;
  uint64_t stack_size;
  uint64_t tls;
  uint64_t set_tid;
  uint64_t set_tid_size;
  uint64_t cgroup;
};
#endif

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
#if __GLIBC__ > 2 || __GLIBC_MINOR__ > 24
    ret = getrandom(buf, len, 0);
#else
    ret = syscall(SYS_getrandom, buf, len, 0);
#endif
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

static int pidfd_send_signal(int pidfd, int sig, siginfo_t *info, unsigned int flags) {
  return syscall(SYS_pidfd_send_signal, pidfd, sig, info, flags);
}

CAMLprim value caml_eio_pidfd_send_signal(value v_pidfd, value v_signal) {
  CAMLparam0();
  int res;

  res = pidfd_send_signal(Int_val(v_pidfd), caml_convert_signal_number(Int_val(v_signal)), NULL, 0);
  if (res == -1) uerror("pidfd_send_signal", Nothing);
  CAMLreturn(Val_unit);
}

static int pidfd_open(pid_t pid, unsigned int flags) {
  return syscall(SYS_pidfd_open, pid, flags);
}

/* Like clone3, but falls back to fork if not supported.
   Also, raises exceptions rather then returning an error. */
static pid_t clone3_with_fallback(struct clone_args *cl_args) {
  int *pidfd = (int *)(uintptr_t) cl_args->pidfd;
  pid_t child_pid = syscall(SYS_clone3, cl_args, sizeof(struct clone_args));

  if (child_pid >= 0)
    return child_pid;		/* Success! */

  if (errno != ENOSYS && errno != EPERM) {
    uerror("clone3", Nothing);	/* Unknown error */
  }

  /* Probably Docker's security policy is blocking clone3. Fall back to forking. */

  child_pid = fork();
  if (child_pid == 0) {
    /* We are the child */
    return 0;
  } else if (child_pid < 0) {
    uerror("fork", Nothing);
  }

  *pidfd = pidfd_open(child_pid, 0);   	/* Is automatically close-on-exec */
  if (*pidfd < 0) {
    int e = errno;
    kill(child_pid, SIGKILL);
    waitpid(child_pid, NULL, 0);
    errno = e;
    uerror("pidfd_open", Nothing);
  }

  return child_pid;
}

CAMLprim value caml_eio_clone3(value v_errors, value v_actions) {
  CAMLparam1(v_actions);
  CAMLlocal1(v_result);
  pid_t child_pid;
  int pidfd = -1;		/* Is automatically close-on-exec */
  struct clone_args cl_args = {
    .flags = CLONE_PIDFD,
    .pidfd = (uintptr_t) &pidfd,
    .exit_signal = SIGCHLD,	/* Needed for wait4 to work if we exit before exec */
    .stack = (uintptr_t) NULL,	/* Use copy-on-write parent stack */
    .stack_size = 0,
  };

  child_pid = clone3_with_fallback(&cl_args);
  if (child_pid == 0) {
    /* Run child actions (doesn't return) */
    eio_unix_run_fork_actions(Int_val(v_errors), v_actions);
  }

  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, Val_long(child_pid));
  Store_field(v_result, 1, Val_int(pidfd));

  CAMLreturn(v_result);
}
