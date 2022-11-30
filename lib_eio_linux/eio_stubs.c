#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/eventfd.h>
#include <sys/random.h>
#include <sys/syscall.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <netdb.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/socketaddr.h>

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

static value caml_unix_cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}

extern int caml_unix_socket_domain_table[]; /* from socket.c */
extern int caml_unix_socket_type_table[];   /* from socket.c */

static value convert_addrinfo(struct addrinfo * a)
{
  CAMLparam0();
  CAMLlocal3(vres,vaddr,vcanonname);
  union sock_addr_union sa;
  socklen_param_type len;

  len = a->ai_addrlen;
  if (len > sizeof(sa)) len = sizeof(sa);
  memcpy(&sa.s_gen, a->ai_addr, len);
  vaddr = caml_unix_alloc_sockaddr(&sa, len, -1);
  vcanonname = caml_copy_string(a->ai_canonname == NULL ? "" : a->ai_canonname);
  vres = caml_alloc_small(5, 0);
  Field(vres, 0) =
   caml_unix_cst_to_constr(a->ai_family, caml_unix_socket_domain_table, 3, 0);
  Field(vres, 1) =
   caml_unix_cst_to_constr(a->ai_socktype, caml_unix_socket_type_table, 4, 0);
  Field(vres, 2) = Val_int(a->ai_protocol);
  Field(vres, 3) = vaddr;
  Field(vres, 4) = vcanonname;
  CAMLreturn(vres);
}

/* glibc doesn't define a bunch of EAI_, so fake one since code gets copied around */

#ifndef EAI_ADDRFAMILY
#define EAI_ADDRFAMILY -3000
#endif /* EAI_ADDRFAMILY */

#ifndef EAI_BADHINTS
#define EAI_BADHINTS -3013
#endif /* EAI_BADHINTS */

#ifndef EAI_NODATA
#define EAI_NODATA -3007
#endif /* EAI_NODATA */

static int gai_errors[] = {
  EAI_ADDRFAMILY,
  EAI_AGAIN,
  EAI_BADFLAGS,
  EAI_BADHINTS,
  EAI_FAIL,
  EAI_FAMILY,
  EAI_MEMORY,
  EAI_NODATA,
  EAI_NONAME,
  EAI_SERVICE,
  EAI_SOCKTYPE,
  EAI_SYSTEM
};

CAMLprim value caml_eio_getaddrinfo(value vnode, value vserv, value vopts)
{
  CAMLparam3(vnode, vserv, vopts);
  CAMLlocal3(vres, v, vret);
  char * node, * serv;
  struct addrinfo hints;
  struct addrinfo * res, * r;
  int retcode, i;

  if (! (caml_string_is_c_safe(vnode) && caml_string_is_c_safe(vserv)))
    CAMLreturn (Val_emptylist);

  /* Extract "node" parameter */
  if (caml_string_length(vnode) == 0) {
    node = NULL;
  } else {
    node = caml_stat_strdup(String_val(vnode));
  }
  /* Extract "service" parameter */
  if (caml_string_length(vserv) == 0) {
    serv = NULL;
  } else {
    serv = caml_stat_strdup(String_val(vserv));
  }
  /* Parse options, set hints */
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; vopts != Val_emptylist; vopts = Field(vopts, 1)) {
    v = Field(vopts, 0);
    if (Is_block(v))
      switch (Tag_val(v)) {
      case 0:                   /* AI_FAMILY of socket_domain */
        hints.ai_family = caml_unix_socket_domain_table[Int_val(Field(v, 0))];
        break;
      case 1:                   /* AI_SOCKTYPE of socket_type */
        hints.ai_socktype = caml_unix_socket_type_table[Int_val(Field(v, 0))];
        break;
      case 2:                   /* AI_PROTOCOL of int */
        hints.ai_protocol = Int_val(Field(v, 0));
        break;
      }
    else
      switch (Int_val(v)) {
      case 0:                   /* AI_NUMERICHOST */
        hints.ai_flags |= AI_NUMERICHOST; break;
      case 1:                   /* AI_CANONNAME */
        hints.ai_flags |= AI_CANONNAME; break;
      case 2:                   /* AI_PASSIVE */
        hints.ai_flags |= AI_PASSIVE; break;
      }
  }
  /* Do the call */
  caml_enter_blocking_section();
  retcode = getaddrinfo(node, serv, &hints, &res);
  caml_leave_blocking_section();
  if (node != NULL) caml_stat_free(node);
  if (serv != NULL) caml_stat_free(serv);
  /* Convert result */
  vres = Val_emptylist;
  if (retcode == 0) {
    for (r = res; r != NULL; r = r->ai_next) {
      v = caml_alloc_small(2, Tag_cons);
      Field(v, 0) = convert_addrinfo(r);
      Field(v, 1) = vres;
      vres = v;
    }
    vret = caml_alloc_small(1, 0); /* 0 = Ok */
    Field(vret, 0) = vres;
    freeaddrinfo(res);
  } else {
    for (i = 0; i < (sizeof(gai_errors) / sizeof(int)); i++)
      if (gai_errors[i] == retcode)
        break;
    if (i == (sizeof(gai_errors) / sizeof(int))) {
      errno = EINVAL;
      uerror("invalid gai_error", Nothing);
    }
    vret = caml_alloc_small(1, 1); /* 1 = Error */
    Field(vret, 0) = Val_int(i);
  }

  CAMLreturn(vret);
}
