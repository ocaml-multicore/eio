#include <unistd.h>
#include <fcntl.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value eio_unix_is_blocking(value v_fd) {
  int fd = Int_val(v_fd);
  int r = fcntl(fd, F_GETFL, 0);
  if (r == -1)
    uerror("fcntl", Nothing);

  return Val_bool((r & O_NONBLOCK) == 0);
}
