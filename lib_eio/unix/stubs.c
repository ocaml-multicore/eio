#include <unistd.h>
#include <fcntl.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value eio_unix_is_blocking(value v_fd) {
  #ifdef _WIN32
  // We should not call this function from Windows
  uerror("Unsupported blocking check on Windows", Nothing);
  #else
  int fd = Int_val(v_fd);
  int r = fcntl(fd, F_GETFL, 0);
  if (r == -1)
    uerror("fcntl", Nothing);

  return Val_bool((r & O_NONBLOCK) == 0);
}
