#include "primitives.h"

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/bigarray.h>

static void caml_stat_free_preserving_errno(void *ptr) {
  int saved = errno;
  caml_stat_free(ptr);
  errno = saved;
}

CAMLprim value eio_unix_is_blocking(value v_fd) {
  #ifdef _WIN32
  // We should not call this function from Windows
  caml_unix_error(EOPNOTSUPP, "Unsupported blocking check on Windows", Nothing);
  #else
  int fd = Int_val(v_fd);
  int r = fcntl(fd, F_GETFL, 0);
  if (r == -1)
    caml_uerror("fcntl", Nothing);

  return Val_bool((r & O_NONBLOCK) == 0);
  #endif
}

CAMLprim value eio_unix_readlinkat(value v_fd, value v_path, value v_cs) {
  #ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "readlinkat not supported on Windows", v_path);
  #else
  CAMLparam2(v_path, v_cs);
  char *path;
  value v_ba = Field(v_cs, 0);
  value v_off = Field(v_cs, 1);
  value v_len = Field(v_cs, 2);
  char *buf = (char *)Caml_ba_data_val(v_ba) + Long_val(v_off); 
  size_t buf_size = Long_val(v_len);
  int fd = Int_val(v_fd);
  int ret;
  caml_unix_check_path(v_path, "readlinkat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = readlinkat(fd, path, buf, buf_size);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) caml_uerror("readlinkat", v_path);
  CAMLreturn(Val_int(ret));
  #endif
}

CAMLprim value eio_unix_fchownat(value v_fd, value v_path, value v_uid, value v_gid, value v_flags) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "fchownat not supported on Windows", v_path);
#else
  CAMLparam3(v_path, v_uid, v_gid);
  char *path;
  int fd = Int_val(v_fd);
  uid_t uid = Int64_val(v_uid);
  gid_t gid = Int64_val(v_gid);
  int ret;
  caml_unix_check_path(v_path, "fchownat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = fchownat(fd, path, uid, gid, Int_val(v_flags));
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1)
    caml_uerror("fchownat", v_path);
  CAMLreturn(Val_unit);
#endif
}

