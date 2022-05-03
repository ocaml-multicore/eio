#include <sys/stat.h>
#include <sys/types.h>
#include <sys/eventfd.h>
#include <sys/random.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

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
