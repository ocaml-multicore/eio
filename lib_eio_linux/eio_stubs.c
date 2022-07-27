#include <sys/stat.h>
#include <sys/types.h>
#include <sys/eventfd.h>
#include <sys/random.h>
#include <sys/syscall.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>


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

CAMLprim value caml_eio_unlinkat(value v_fd, value v_path, value v_rmdir) {
  CAMLparam1(v_path);
  char *path;
  int ret;
  caml_unix_check_path(v_path, "unlinkat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = unlinkat(Int_val(v_fd), path,
                 Bool_val(v_rmdir) ? AT_REMOVEDIR : 0);
  caml_leave_blocking_section();
  caml_stat_free(path);
  if (ret == -1) uerror("unlinkat", v_path);
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
