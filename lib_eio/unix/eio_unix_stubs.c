#include "eio_config.h"

#if defined(EIO_ON_POSIX)

#include <caml/mlvalues.h>
#include <sys/socket.h>

value /* noalloc */
caml_eio_somaxconn(value unit)
{
  return Val_int(SOMAXCONN);
}

#endif

#if defined(EIO_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <winsock2.h>

value caml_eio_somaxconn(value unit)
{
  return Val_int(SOMAXCONN);
}

#endif
