#include <stdlib.h>
#include <unistd.h>

#ifndef _WIN32
#include <sys/socket.h>
#endif

#ifdef __linux__
#include <netinet/in.h>
#include <netinet/tcp.h>
#endif

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#ifndef TCP_CORK
#define TCP_CORK (-1)
#endif

#ifndef TCP_KEEPCNT
#define TCP_KEEPCNT (-1)
#endif

#ifndef TCP_KEEPIDLE
#define TCP_KEEPIDLE (-1)
#endif

#ifndef TCP_KEEPINTVL
#define TCP_KEEPINTVL (-1)
#endif

#ifndef TCP_DEFER_ACCEPT
#define TCP_DEFER_ACCEPT (-1)
#endif

#ifndef TCP_NODELAY
#define TCP_NODELAY (-1)
#endif

struct socket_option {
  int level;
  int option;
};

/* Not exported by caml/sockaddr.h */
CAMLexport value caml_unix_getsockopt_aux(char *, int, int, int, value);
CAMLexport value caml_unix_setsockopt_aux(char *, int, int, int, value, value);

static struct socket_option sockopt_int[] = {
  { IPPROTO_TCP, TCP_CORK },
  { IPPROTO_TCP, TCP_KEEPCNT },
  { IPPROTO_TCP, TCP_KEEPIDLE },
  { IPPROTO_TCP, TCP_KEEPINTVL },
  { IPPROTO_TCP, TCP_DEFER_ACCEPT },
  { IPPROTO_TCP, TCP_NODELAY },
};

CAMLprim value eio_unix_getsockopt_int(value vsocket, value voption)
{
  struct socket_option *opt = &(sockopt_int[Int_val(voption)]);
  return caml_unix_getsockopt_aux("eio_unix_getsockopt_int",
                             1, /* TYPE_INT */
                             opt->level,
                             opt->option,
                             vsocket);
}

CAMLprim value eio_unix_setsockopt_int(value vsocket, value voption, value val)
{
  struct socket_option *opt = &(sockopt_int[Int_val(voption)]);
  return caml_unix_setsockopt_aux("eio_unix_setsockopt_int",
                             1, /* TYPE_INT */
                             opt->level,
                             opt->option,
                             vsocket,
                             val);
}
