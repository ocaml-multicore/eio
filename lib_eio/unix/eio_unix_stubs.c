#include "primitives.h"

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
#else
# include <sys/socket.h>
# include <netdb.h>
# include <netinet/tcp.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#ifndef _WIN32
# include <caml/socketaddr.h>
#endif

#ifdef _WIN32
CAMLnoret CAMLextern
void caml_unix_error (int errcode, const char * cmdname, value arg);
CAMLnoret CAMLextern
void caml_uerror (const char * cmdname, value arg);
#endif

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

CAMLprim value eio_unix_file_type_of_dtype (int d_type) {
  #ifdef _WIN32
  return caml_hash_variant("Unknown");
  #else
  switch (d_type) {
    case DT_REG: return caml_hash_variant("Regular_file");
    case DT_DIR: return caml_hash_variant("Directory");
    case DT_CHR: return caml_hash_variant("Character_special");
    case DT_BLK: return caml_hash_variant("Block_device");
	case DT_LNK: return caml_hash_variant("Symbolic_link");
	case DT_FIFO: return caml_hash_variant("Fifo");
	case DT_SOCK: return caml_hash_variant("Socket");
	default:
      return caml_hash_variant("Unknown");
  }
  #endif
}

CAMLprim value eio_unix_fchmodat(value v_fd, value v_path, value v_mode, value v_flags) {
  #ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "fchmodat not supported on Windows", v_path);
  #else
  CAMLparam1(v_path);
  char *path;
  int ret;
  caml_unix_check_path(v_path, "fchmodat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = fchmodat(Int_val(v_fd), path, Int_val(v_mode), Int_val(v_flags));
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) caml_uerror("fchmodat", v_path);
  CAMLreturn(Val_unit);
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

#ifndef _WIN32
static int caml_eai_of_unix(int eai) {
  switch (eai) {
    // These numbers must match the order of constructors in Eio.Net.Getaddrinfo_error.t
#ifdef EAI_ADDRFAMILY
    case EAI_ADDRFAMILY: return 1;
#endif
    case EAI_AGAIN: return 2;
    case EAI_BADFLAGS: return 3;
#ifdef EAI_BADHINTS
    case EAI_BADHINTS: return 4;
#endif
    case EAI_FAIL: return 5;
    case EAI_FAMILY: return 6;
    case EAI_MEMORY: return 7;
#ifdef EAI_NODATA
    case EAI_NODATA: return 8;
#endif
    case EAI_NONAME: return 9;
#ifdef EAI_OVERFLOW
    case EAI_OVERFLOW: return 10;
#endif
#ifdef EAI_PROTOCOL
    case EAI_PROTOCOL: return 11;
#endif
    case EAI_SERVICE: return 12;
    case EAI_SOCKTYPE: return 13;
    default: return 0;
  }
}
#endif /* !_WIN32 */

CAMLprim value eio_unix_getaddrinfo(value v_node, value v_service) {
#ifdef _WIN32
  caml_unix_error(EOPNOTSUPP, "getaddrinfo not supported on Windows", Nothing);
#else
  CAMLparam2(v_node, v_service);
  CAMLlocal3(v_result, v_list, v_cons);
  CAMLlocal3(v_host, v_addr, v_item);
  struct addrinfo *res = NULL;
  int r;
  int have_node = caml_string_length(v_node) > 0;
  int have_service = caml_string_length(v_service) > 0;
  int saved_errno = 0;

  {
    struct addrinfo hints = {
      .ai_family = AF_UNSPEC,
      .ai_socktype = 0,
      .ai_protocol = 0,
      .ai_flags = AI_ADDRCONFIG,
    };
    char *node = have_node ? caml_stat_strdup(String_val(v_node)) : NULL;
    char *service = have_service ? caml_stat_strdup(String_val(v_service)) : NULL;

    caml_enter_blocking_section();
    r = getaddrinfo(node, service, &hints, &res);
    saved_errno = errno;
    caml_leave_blocking_section();

    if (node) caml_stat_free(node);
    if (service) caml_stat_free(service);
  }

  if (r == 0) {
    struct addrinfo *item;

    for (item = res; item; item = item->ai_next) {
      if (item->ai_socktype == SOCK_STREAM || item->ai_socktype == SOCK_DGRAM) {
        int tag, port;

        switch (item->ai_protocol) {
          case IPPROTO_TCP:
            tag = caml_hash_variant("Tcp");
            break;
          case IPPROTO_UDP:
            tag = caml_hash_variant("Udp");
            break;
          default:
            continue;
        }

        switch (item->ai_family) {
          case AF_INET: {
            struct sockaddr_in *ip = (struct sockaddr_in *) item->ai_addr;
            v_host = caml_unix_alloc_inet_addr(&ip->sin_addr);
            port = ip->sin_port;
            break;
          }
          case AF_INET6: {
            struct sockaddr_in6 *ip6 = (struct sockaddr_in6 *) item->ai_addr;
            v_host = caml_unix_alloc_inet6_addr(&ip6->sin6_addr);
            port = ip6->sin6_port;
            break;
          }
          default:
            continue;
        }

        if (!have_service) { port = 0; }

        v_addr = caml_alloc(2, 0);
        Store_field(v_addr, 0, v_host);
        Store_field(v_addr, 1, Val_int(ntohs(port)));

        v_item = caml_alloc(2, 0);
        Store_field(v_item, 0, tag);
        Store_field(v_item, 1, v_addr);

        v_cons = caml_alloc(2, Tag_cons);
        Store_field(v_cons, 0, v_item);
        Store_field(v_cons, 1, v_list);
        v_list = v_cons;
      }
    }

    freeaddrinfo(res);

    v_result = caml_alloc(1, 0);        // Ok res
    Store_field(v_result, 0, v_list);
    CAMLreturn(v_result);
  } else if (r == EAI_SYSTEM) {
    errno = saved_errno;
    caml_uerror("getaddrinfo", v_node);
  } else {
    v_result = caml_alloc(1, 1);        // Error r
    Store_field(v_result, 0, Val_int(caml_eai_of_unix(r)));
    CAMLreturn(v_result);
  }
#endif
}

typedef struct _sockopt {
  int level;
  int optname;
} sockopt;

// Keep in sync with Sockopt.t
static sockopt find_sockopt(value v_id) {
  switch (Int_val(v_id)) {
#ifdef TCP_CORK
    case 0: return (sockopt){ IPPROTO_TCP, TCP_CORK };
#endif
#ifdef TCP_QUICKACK
    case 1: return (sockopt){ IPPROTO_TCP, TCP_QUICKACK };
#endif
#ifdef IP_FREEBIND
    case 2: return (sockopt){ IPPROTO_IP, IP_FREEBIND };
#endif
#ifdef IP_BIND_ADDRESS_NO_PORT
    case 3: return (sockopt){ IPPROTO_IP, IP_BIND_ADDRESS_NO_PORT };
#endif
#ifdef TCP_KEEPIDLE
    case 4: return (sockopt){ IPPROTO_TCP, TCP_KEEPIDLE };
#endif
#ifdef TCP_KEEPINTVL
    case 5: return (sockopt){ IPPROTO_TCP, TCP_KEEPINTVL };
#endif
#ifdef TCP_KEEPCNT
    case 6: return (sockopt){ IPPROTO_TCP, TCP_KEEPCNT };
#endif
#ifdef TCP_USER_TIMEOUT
    case 7: return (sockopt){ IPPROTO_TCP, TCP_USER_TIMEOUT };
#endif
#ifdef TCP_MAXSEG
    case 8: return (sockopt){ IPPROTO_TCP, TCP_MAXSEG };
#endif
#ifdef TCP_DEFER_ACCEPT
    case 9: return (sockopt){ IPPROTO_TCP, TCP_DEFER_ACCEPT };
#endif
#ifdef TCP_SYNCNT
    case 10: return (sockopt){ IPPROTO_TCP, TCP_SYNCNT };
#endif
#ifdef TCP_WINDOW_CLAMP
    case 11: return (sockopt){ IPPROTO_TCP, TCP_WINDOW_CLAMP };
#endif
#ifdef TCP_FASTOPEN
    case 12: return (sockopt){ IPPROTO_TCP, TCP_FASTOPEN };
#endif
#ifdef IP_LOCAL_PORT_RANGE
    case 13: return (sockopt){ IPPROTO_IP, IP_LOCAL_PORT_RANGE };
#endif
#ifdef IP_MTU_DISCOVER
    case 14: return (sockopt){ IPPROTO_IP, IP_MTU_DISCOVER };
#endif
#ifdef IP_TTL
    case 15: return (sockopt){ IPPROTO_IP, IP_TTL };
#endif
#ifdef IP_MTU
    case 16: return (sockopt){ IPPROTO_IP, IP_MTU };
#endif
#ifdef TCP_LINGER2
    case 17: return (sockopt){ IPPROTO_TCP, TCP_LINGER2 };
#endif
#ifdef TCP_CONGESTION
    case 18: return (sockopt){ IPPROTO_TCP, TCP_CONGESTION };
#endif
  }
  caml_raise_not_found();
}

CAMLprim value caml_eio_sockopt_int_set(value v_fd, value v_id, value v_val) {
  CAMLparam3(v_fd, v_id, v_val);
  sockopt opt = find_sockopt(v_id);
  int ret;
  int val = Int_val(v_val);
#ifdef _WIN32
  SOCKET fd = Socket_val(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  ret = setsockopt(fd, opt.level, opt.optname, (void *) &val, sizeof(val));
  if (ret == -1) caml_uerror("setsockopt", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_sockopt_int_get(value v_fd, value v_id) {
  CAMLparam2(v_fd, v_id);
  sockopt opt = find_sockopt(v_id);
  int ret;
  int val;
#ifdef _WIN32
  SOCKET fd = Socket_val(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  socklen_t len = sizeof(val);
  ret = getsockopt(fd, opt.level, opt.optname, (void *) &val, &len);
  if (ret == -1) caml_uerror("getsockopt", Nothing);
  CAMLreturn(Val_int(val));
}

CAMLprim value caml_eio_sockopt_string_set(value v_fd, value v_id, value v_val) {
  CAMLparam3(v_fd, v_id, v_val);
  sockopt opt = find_sockopt(v_id);
#ifdef _WIN32
  SOCKET fd = Socket_val(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  const char *str = String_val(v_val);
  int ret = setsockopt(fd, opt.level, opt.optname, (void *) str, caml_string_length(v_val));
  if (ret == -1) caml_uerror("setsockopt", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_sockopt_string_get(value v_fd, value v_id) {
  CAMLparam2(v_fd, v_id);
  CAMLlocal1(v_result);
  sockopt opt = find_sockopt(v_id);
#ifdef _WIN32
  SOCKET fd = Socket_val(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  char buffer[256];
  socklen_t len = sizeof(buffer);
  int ret = getsockopt(fd, opt.level, opt.optname, (void *) buffer, &len);
  if (ret == -1) caml_uerror("getsockopt", Nothing);
  // At least for TCP_CONGESTION, Linux sets the returned length to min(sizeof(buffer), TCP_CA_NAME_MAX))
  // (16 bytes), even if the actual string is shorter. Therefore, we need to calculate the actual length.
  // Probably any possible result will fit in 16 bytes including the '\0' terminator, but to be safe
  // we avoid reading out-of-bounds even if we don't get a terminator.
  size_t slen = strnlen(buffer, len);
  if (slen == sizeof(buffer))
    caml_invalid_argument("getsockopt: buffer was too small!");
  v_result = caml_alloc_initialized_string(slen, buffer);
  CAMLreturn(v_result);
}

CAMLprim value caml_eio_unix_so_type(value v_fd) {
  CAMLparam1(v_fd);
  int ty;
  socklen_t optlen = sizeof(ty);
  #ifdef _WIN32
  SOCKET sock = Socket_val(v_fd);
  if (getsockopt(sock, SOL_SOCKET, SO_TYPE, (char *)&ty, &optlen) != 0) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("getsockopt", Nothing);
  }
  #else
  int sock = Int_val(v_fd);
  if (getsockopt(sock, SOL_SOCKET, SO_TYPE, &ty, &optlen) != 0)
    caml_uerror("getsockopt", Nothing);
  #endif
  switch (ty) {
    case SOCK_STREAM:    CAMLreturn(caml_hash_variant("Stream"));
    case SOCK_DGRAM:     CAMLreturn(caml_hash_variant("Dgram"));
    case SOCK_RAW:       CAMLreturn(caml_hash_variant("Raw"));
  #ifdef SOCK_SEQPACKET
    case SOCK_SEQPACKET: CAMLreturn(caml_hash_variant("Seqpacket"));
  #endif
    default:             caml_invalid_argument("Eio_unix: unrecognised SO_TYPE");
  }
  CAMLreturn(Val_unit); /* unreachable */
}
