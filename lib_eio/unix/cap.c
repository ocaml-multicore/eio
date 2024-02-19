#include "primitives.h"

#include <errno.h>
#include <sys/param.h>

#ifdef __FreeBSD__
# define HAVE_CAPSICUM
#endif

#ifdef HAVE_CAPSICUM
# include <sys/capsicum.h>
#endif

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value eio_unix_cap_enter(value v_unit) {
#ifdef HAVE_CAPSICUM
  int r = cap_enter();
  if (r == -1 && errno != ENOSYS)
    caml_uerror("cap_enter", Nothing);

  return Val_bool(r == 0);
#else
  return Val_bool(0);
#endif
}
