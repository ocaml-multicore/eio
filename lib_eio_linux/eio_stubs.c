#include <sys/stat.h>
#include <sys/types.h>
#include <sys/eventfd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

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
