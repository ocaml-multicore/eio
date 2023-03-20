#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#ifdef __linux__
#include <sys/random.h>
#endif
#include <sys/uio.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#include "fork_action.h"

#ifdef ARCH_SIXTYFOUR
#define Int63_val(v) Long_val(v)
#else
#define Int63_val(v) (Int64_val(v)) >> 1
#endif

static void caml_stat_free_preserving_errno(void *ptr) {
  int saved = errno;
  caml_stat_free(ptr);
  errno = saved;
}

CAMLprim value caml_eio_posix_getrandom(value v_ba, value v_off, value v_len) {
  CAMLparam1(v_ba);
  ssize_t ret;
  ssize_t off = (ssize_t)Long_val(v_off);
  ssize_t len = (ssize_t)Long_val(v_len);
  do {
    void *buf = (uint8_t *)Caml_ba_data_val(v_ba) + off;
    caml_enter_blocking_section();
#ifdef __linux__
    ret = getrandom(buf, len, 0);
#else
    arc4random_buf(buf, len);
    ret = len;
#endif
    caml_leave_blocking_section();
  } while (ret == -1 && errno == EINTR);
  if (ret == -1) uerror("getrandom", Nothing);
  CAMLreturn(Val_long(ret));
}

/* Fill [iov] with pointers to the cstructs in the array [v_bufs]. */
static void fill_iov(struct iovec *iov, value v_bufs) {
  int n_bufs = Wosize_val(v_bufs);
  for (int i = 0; i < n_bufs; i++) {
    value v_cs = Field(v_bufs, i);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iov[i].iov_base = (uint8_t *)Caml_ba_data_val(v_ba) + Long_val(v_off);
    iov[i].iov_len = Long_val(v_len);
  }
}

CAMLprim value caml_eio_posix_readv(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  r = readv(Int_val(v_fd), iov, n_bufs);
  if (r < 0) uerror("readv", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_writev(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  r = writev(Int_val(v_fd), iov, n_bufs);
  if (r < 0) uerror("writev", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_preadv(value v_fd, value v_bufs, value v_offset) {
  CAMLparam2(v_bufs, v_offset);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  r = preadv(Int_val(v_fd), iov, n_bufs, Int63_val(v_offset));
  if (r < 0) uerror("preadv", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_pwritev(value v_fd, value v_bufs, value v_offset) {
  CAMLparam2(v_bufs, v_offset);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec iov[n_bufs];

  fill_iov(iov, v_bufs);

  r = pwritev(Int_val(v_fd), iov, n_bufs, Int63_val(v_offset));
  if (r < 0) uerror("pwritev", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_openat(value v_dirfd, value v_pathname, value v_flags, value v_mode) {
  CAMLparam1(v_pathname);
  char* pathname;
  int r;

  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup(String_val(v_pathname));

  caml_enter_blocking_section();
  r = openat(Int_val(v_dirfd), pathname, Int_val(v_flags), Int_val(v_mode));
  caml_leave_blocking_section();

  caml_stat_free_preserving_errno(pathname);
  if (r < 0) uerror("openat", v_pathname);
  CAMLreturn(Val_int(r));
}

CAMLprim value caml_eio_posix_mkdirat(value v_fd, value v_path, value v_perm) {
  CAMLparam1(v_path);
  char *path;
  int ret;
  caml_unix_check_path(v_path, "mkdirat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = mkdirat(Int_val(v_fd), path, Int_val(v_perm));
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) uerror("mkdirat", v_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_unlinkat(value v_fd, value v_path, value v_dir) {
  CAMLparam1(v_path);
  char *path;
  int flags = Bool_val(v_dir) ? AT_REMOVEDIR : 0;
  int ret;
  caml_unix_check_path(v_path, "unlinkat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = unlinkat(Int_val(v_fd), path, flags);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) uerror("unlinkat", v_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path) {
  CAMLparam2(v_old_path, v_new_path);
  size_t old_path_len = caml_string_length(v_old_path);
  size_t new_path_len = caml_string_length(v_new_path);
  char *old_path;
  char *new_path;
  int ret;
  caml_unix_check_path(v_old_path, "renameat-old");
  caml_unix_check_path(v_new_path, "renameat-new");
  old_path = caml_stat_alloc(old_path_len + new_path_len + 2);
  new_path = old_path + old_path_len + 1;
  memcpy(old_path, String_val(v_old_path), old_path_len + 1);
  memcpy(new_path, String_val(v_new_path), new_path_len + 1);
  caml_enter_blocking_section();
  ret = renameat(Int_val(v_old_fd), old_path,
                 Int_val(v_new_fd), new_path);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(old_path);
  if (ret == -1) uerror("renameat", v_old_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_spawn(value v_errors, value v_actions) {
  CAMLparam1(v_actions);
  pid_t child_pid;

  child_pid = fork();
  if (child_pid == 0) {
    eio_unix_run_fork_actions(Int_val(v_errors), v_actions);
  } else if (child_pid < 0) {
    uerror("fork", Nothing);
  }

  CAMLreturn(Val_long(child_pid));
}
