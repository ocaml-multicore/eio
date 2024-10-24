#include "primitives.h"

#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/socket.h>
#ifdef __linux__
#if __GLIBC__ > 2 || __GLIBC_MINOR__ > 24
#include <sys/random.h>
#else
#include <sys/syscall.h>
#endif
#endif
#include <sys/uio.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/socketaddr.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "fork_action.h"

#ifdef ARCH_SIXTYFOUR
#define Int63_val(v) Long_val(v)
#define caml_copy_int63(v) Val_long(v)
#else
#define Int63_val(v) (Int64_val(v)) >> 1
#define caml_copy_int63(v) caml_copy_int64(v << 1)
#endif

static void caml_stat_free_preserving_errno(void *ptr) {
  int saved = errno;
  caml_stat_free(ptr);
  errno = saved;
}

CAMLprim value caml_eio_posix_getrandom(value v_ba, value v_off, value v_len) {
  CAMLparam1(v_ba);
  ssize_t ret;
  ssize_t off = (ssize_t)Long_val(v_off);
  ssize_t len = (ssize_t)Long_val(v_len);
  do {
    void *buf = (uint8_t *)Caml_ba_data_val(v_ba) + off;
    caml_enter_blocking_section();
#ifdef __linux__
#if __GLIBC__ > 2 || __GLIBC_MINOR__ > 24
    ret = getrandom(buf, len, 0);
#else
    ret = syscall(SYS_getrandom, buf, len, 0);
#endif
#else
    arc4random_buf(buf, len);
    ret = len;
#endif
    caml_leave_blocking_section();
  } while (ret == -1 && errno == EINTR);
  if (ret == -1) uerror("getrandom", Nothing);
  CAMLreturn(Val_long(ret));
}

/* Allocates an array of C iovecs using the cstructs in the array [v_bufs]. */
static struct iovec *alloc_iov(value v_bufs) {
  struct iovec *iov;
  int n_bufs = Wosize_val(v_bufs);

  if (n_bufs == 0) return NULL;
  iov = caml_stat_calloc_noexc(n_bufs, sizeof(struct iovec));
  if (iov == NULL)
    caml_raise_out_of_memory();

  for (int i = 0; i < n_bufs; i++) {
    value v_cs = Field(v_bufs, i);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iov[i].iov_base = (uint8_t *)Caml_ba_data_val(v_ba) + Long_val(v_off);
    iov[i].iov_len = Long_val(v_len);
  }
  return iov;
}

CAMLprim value caml_eio_posix_readv(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec *iov;

  iov = alloc_iov(v_bufs);
  caml_enter_blocking_section();
  r = readv(Int_val(v_fd), iov, n_bufs);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("readv", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_writev(value v_fd, value v_bufs) {
  CAMLparam1(v_bufs);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec *iov;

  iov = alloc_iov(v_bufs);
  caml_enter_blocking_section();
  r = writev(Int_val(v_fd), iov, n_bufs);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("writev", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_preadv(value v_fd, value v_bufs, value v_offset) {
  CAMLparam2(v_bufs, v_offset);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec *iov;
  off_t offset = Int63_val(v_offset);

  iov = alloc_iov(v_bufs);
  caml_enter_blocking_section();
  r = preadv(Int_val(v_fd), iov, n_bufs, offset);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("preadv", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_pwritev(value v_fd, value v_bufs, value v_offset) {
  CAMLparam2(v_bufs, v_offset);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec *iov;
  off_t offset = Int63_val(v_offset);

  iov = alloc_iov(v_bufs);
  caml_enter_blocking_section();
  r = pwritev(Int_val(v_fd), iov, n_bufs, offset);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("pwritev", Nothing);

  CAMLreturn(Val_long(r));
}

CAMLprim value caml_eio_posix_openat(value v_dirfd, value v_pathname, value v_flags, value v_mode) {
  CAMLparam1(v_pathname);
  char* pathname;
  int r;

  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup(String_val(v_pathname));

  caml_enter_blocking_section();
  r = openat(Int_val(v_dirfd), pathname, Int_val(v_flags), Int_val(v_mode));
  caml_leave_blocking_section();

  caml_stat_free_preserving_errno(pathname);
  if (r < 0) uerror("openat", v_pathname);
  CAMLreturn(Val_int(r));
}

CAMLprim value caml_eio_posix_mkdirat(value v_fd, value v_path, value v_perm) {
  CAMLparam1(v_path);
  char *path;
  int ret;
  caml_unix_check_path(v_path, "mkdirat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = mkdirat(Int_val(v_fd), path, Int_val(v_perm));
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) uerror("mkdirat", v_path);
  CAMLreturn(Val_unit);
}

#define Stat_val(v) (*((struct stat **) Data_custom_val(v)))

static void finalize_stat(value v) {
  caml_stat_free(Stat_val(v));
  Stat_val(v) = NULL;
}

static struct custom_operations stat_ops = {
  "eio_posix.stat",
  finalize_stat,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value
caml_eio_posix_make_stat(value v_unit) {
  CAMLparam0();
  CAMLlocal1(v);
  struct stat *data;
  v = caml_alloc_custom_mem(&stat_ops, sizeof(struct stat *), sizeof(struct stat));
  Stat_val(v) = NULL;
  data = (struct stat *) caml_stat_alloc(sizeof(struct stat));
  Stat_val(v) = data;
  CAMLreturn(v);
}

static value get_file_type_variant(struct stat *sb) {
  int filetype = sb->st_mode & S_IFMT;
  switch (filetype) {
    case S_IFREG:
      return caml_hash_variant("Regular_file");
    case S_IFSOCK:
      return caml_hash_variant("Socket");
    case S_IFLNK:
      return caml_hash_variant("Symbolic_link");
    case S_IFBLK:
      return caml_hash_variant("Block_device");
    case S_IFDIR:
      return caml_hash_variant("Directory");
    case S_IFCHR:
      return caml_hash_variant("Character_special");
    case S_IFIFO:
      return caml_hash_variant("Fifo");
    default:
      return caml_hash_variant("Unknown");
  }
}

CAMLprim value caml_eio_posix_fstatat(value v_stat, value v_fd, value v_path, value v_flags) {
  CAMLparam2(v_stat, v_path);
  char *path;
  int ret;
  struct stat *statbuf = Stat_val(v_stat);
  bzero(statbuf, sizeof(struct stat));
  caml_unix_check_path(v_path, "fstatat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = fstatat(Int_val(v_fd), path, statbuf, Int_val(v_flags));
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) uerror("fstatat", v_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_fstat(value v_stat, value v_fd) {
  CAMLparam1(v_stat);
  int ret;
  struct stat *statbuf = Stat_val(v_stat);
  bzero(statbuf, sizeof(struct stat));
  caml_enter_blocking_section();
  ret = fstat(Int_val(v_fd), statbuf);
  caml_leave_blocking_section();
  if (ret == -1) uerror("fstat", Nothing);
  CAMLreturn(Val_unit);
}

// Non-allocating (for native mode) accessors for struct stat
#define STAT_GETTER(field, return_type, ocaml_value_maker) \
return_type ocaml_eio_posix_stat_##field##_native(value v_stat) { \
  struct stat *s = Stat_val(v_stat); \
  return s->st_##field; \
} \
value ocaml_eio_posix_stat_ ## field ## _bytes(value v_stat) { \
  return ocaml_value_maker(ocaml_eio_posix_stat_##field##_native(v_stat)); \
}

STAT_GETTER(blksize, int64_t, caml_copy_int64)
STAT_GETTER(nlink, int64_t, caml_copy_int64)
STAT_GETTER(uid, int64_t, caml_copy_int64)
STAT_GETTER(gid, int64_t, caml_copy_int64)
STAT_GETTER(ino, int64_t, caml_copy_int64)
STAT_GETTER(size, int64_t, caml_copy_int64)
STAT_GETTER(blocks, int64_t, caml_copy_int64)
STAT_GETTER(mode, intnat, Val_int)

#define STAT_TIME_GETTER(name,field) \
int64_t ocaml_eio_posix_stat_##name##_sec_native(value v_stat) { \
  struct stat *s = Stat_val(v_stat); \
  return s->st_##field.tv_sec; \
} \
value ocaml_eio_posix_stat_##name##_sec_bytes(value v_stat) { \
  return caml_copy_int64(ocaml_eio_posix_stat_##name##_sec_native(v_stat)); \
} \
value ocaml_eio_posix_stat_##name##_nsec(value v_stat) { \
  struct stat *s = Stat_val(v_stat); \
  return Val_int(s->st_##field.tv_nsec); \
}

#ifdef __APPLE__
STAT_TIME_GETTER(atime,atimespec)
STAT_TIME_GETTER(ctime,ctimespec)
STAT_TIME_GETTER(mtime,mtimespec)
#else
STAT_TIME_GETTER(atime,atim)
STAT_TIME_GETTER(ctime,ctim)
STAT_TIME_GETTER(mtime,mtim)
#endif

intnat
ocaml_eio_posix_stat_perm_native(value v_stat) {
  struct stat *s = Stat_val(v_stat);
  return (s->st_mode & ~S_IFMT);
}

value
ocaml_eio_posix_stat_perm_bytes(value v_stat) {
  return Val_int(ocaml_eio_posix_stat_perm_native(v_stat));
}

value
ocaml_eio_posix_stat_kind(value v_stat) {
  struct stat *s = Stat_val(v_stat);
  return get_file_type_variant(s);
}

int64_t
ocaml_eio_posix_stat_rdev_native(value v_stat) {
  struct stat *s = Stat_val(v_stat);
  return s->st_rdev;
}

value
ocaml_eio_posix_stat_rdev_bytes(value v_stat) {
  return caml_copy_int64(ocaml_eio_posix_stat_rdev_native(v_stat));
}

int64_t
ocaml_eio_posix_stat_dev_native(value v_stat) {
  struct stat *s = Stat_val(v_stat);
  return s->st_dev;
}

value
ocaml_eio_posix_stat_dev_bytes(value v_stat) {
  return caml_copy_int64(ocaml_eio_posix_stat_dev_native(v_stat));
}

CAMLprim value caml_eio_posix_unlinkat(value v_fd, value v_path, value v_dir) {
  CAMLparam1(v_path);
  char *path;
  int flags = Bool_val(v_dir) ? AT_REMOVEDIR : 0;
  int ret;
  caml_unix_check_path(v_path, "unlinkat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = unlinkat(Int_val(v_fd), path, flags);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) uerror("unlinkat", v_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path) {
  CAMLparam2(v_old_path, v_new_path);
  size_t old_path_len = caml_string_length(v_old_path);
  size_t new_path_len = caml_string_length(v_new_path);
  char *old_path;
  char *new_path;
  int ret;
  caml_unix_check_path(v_old_path, "renameat-old");
  caml_unix_check_path(v_new_path, "renameat-new");
  old_path = caml_stat_alloc(old_path_len + new_path_len + 2);
  new_path = old_path + old_path_len + 1;
  memcpy(old_path, String_val(v_old_path), old_path_len + 1);
  memcpy(new_path, String_val(v_new_path), new_path_len + 1);
  caml_enter_blocking_section();
  ret = renameat(Int_val(v_old_fd), old_path,
                 Int_val(v_new_fd), new_path);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(old_path);
  if (ret == -1) uerror("renameat", v_old_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_symlinkat(value v_old_path, value v_new_fd, value v_new_path) {
  CAMLparam2(v_old_path, v_new_path);
  size_t old_path_len = caml_string_length(v_old_path);
  size_t new_path_len = caml_string_length(v_new_path);
  char *old_path;
  char *new_path;
  int ret;
  caml_unix_check_path(v_old_path, "symlinkat-old");
  caml_unix_check_path(v_new_path, "symlinkat-new");
  old_path = caml_stat_alloc(old_path_len + new_path_len + 2);
  new_path = old_path + old_path_len + 1;
  memcpy(old_path, String_val(v_old_path), old_path_len + 1);
  memcpy(new_path, String_val(v_new_path), new_path_len + 1);
  caml_enter_blocking_section();
  ret = symlinkat(old_path, Int_val(v_new_fd), new_path);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(old_path);
  if (ret == -1) uerror("symlinkat", v_old_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_fchmodat(value v_fd, value v_path, value v_mode, value v_flags) {
  CAMLparam1(v_path);
  char *path;
  int ret;
  caml_unix_check_path(v_path, "fchmodat");
  path = caml_stat_strdup(String_val(v_path));
  caml_enter_blocking_section();
  ret = fchmodat(Int_val(v_fd), path, Int_val(v_mode), Int_val(v_flags));
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(path);
  if (ret == -1) uerror("fchmodat", v_path);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_posix_spawn(value v_errors, value v_actions) {
  CAMLparam1(v_actions);
  pid_t child_pid;

  child_pid = fork();
  if (child_pid == 0) {
    eio_unix_run_fork_actions(Int_val(v_errors), v_actions);
  } else if (child_pid < 0) {
    uerror("fork", Nothing);
  }

  CAMLreturn(Val_long(child_pid));
}

/* Copy [n_fds] from [v_fds] to [msg]. */
static void fill_fds(struct msghdr *msg, int n_fds, value v_fds) {
  if (n_fds > 0) {
    int i;
    struct cmsghdr *cm;
    cm = CMSG_FIRSTHDR(msg);
    cm->cmsg_level = SOL_SOCKET;
    cm->cmsg_type = SCM_RIGHTS;
    cm->cmsg_len = CMSG_LEN(n_fds * sizeof(int));
    for (i = 0; i < n_fds; i++) {
      int fd = -1;
      if (Is_block(v_fds)) {
	fd = Int_val(Field(v_fds, 0));
	v_fds = Field(v_fds, 1);
      }
      ((int *)CMSG_DATA(cm))[i] = fd;
    }
  }
}

CAMLprim value caml_eio_posix_send_msg(value v_fd, value v_n_fds, value v_fds, value v_dst_opt, value v_bufs) {
  CAMLparam3(v_fds, v_dst_opt, v_bufs);
  int n_bufs = Wosize_val(v_bufs);
  int n_fds = Int_val(v_n_fds);
  union sock_addr_union dst_addr;
  struct iovec *iov;
  int controllen = n_fds > 0 ? CMSG_SPACE(sizeof(int) * n_fds) : 0;
  char cmsg[controllen];
  struct msghdr msg = {
    .msg_iovlen = n_bufs,
    .msg_control = n_fds > 0 ? cmsg : NULL,
    .msg_controllen = controllen,
  };
  ssize_t r;

  memset(cmsg, 0, controllen);

  if (Is_some(v_dst_opt)) {
    caml_unix_get_sockaddr(Some_val(v_dst_opt), &dst_addr, &msg.msg_namelen);
    msg.msg_name = &dst_addr;
  }

  iov = alloc_iov(v_bufs);
  msg.msg_iov = iov;
  fill_fds(&msg, n_fds, v_fds);

  caml_enter_blocking_section();
  r = sendmsg(Int_val(v_fd), &msg, 0);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("send_msg", Nothing);

  CAMLreturn(Val_long(r));
}

static value get_msghdr_fds(struct msghdr *msg) {
  CAMLparam0();
  CAMLlocal2(v_list, v_cons);
  struct cmsghdr *cm;
  v_list = Val_int(0);
  for (cm = CMSG_FIRSTHDR(msg); cm; cm = CMSG_NXTHDR(msg, cm)) {
    if (cm->cmsg_level == SOL_SOCKET && cm->cmsg_type == SCM_RIGHTS) {
      int *fds = (int *) CMSG_DATA(cm);
      int n_fds = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);
      int i;
      for (i = n_fds - 1; i >= 0; i--) {
	value fd = Val_int(fds[i]);
	v_cons = caml_alloc_tuple(2);
	Store_field(v_cons, 0, fd);
	Store_field(v_cons, 1, v_list);
	v_list = v_cons;
      }
    }
  }
  CAMLreturn(v_list);
}

/* Work-around for https://github.com/ocaml/ocaml/issues/12796 */
static value safe_caml_unix_alloc_sockaddr(union sock_addr_union *adr, socklen_param_type adr_len, int close_on_error) {
  struct sockaddr_un empty = {
    .sun_family = AF_UNIX,
    .sun_path = "",
  };

  if (adr_len < offsetof(struct sockaddr, sa_data)) {
    adr = (union sock_addr_union *) &empty;
    adr_len = offsetof(struct sockaddr, sa_data);
  }

  return caml_unix_alloc_sockaddr(adr, adr_len, close_on_error);
}

CAMLprim value caml_eio_posix_recv_msg(value v_fd, value v_max_fds, value v_bufs) {
  CAMLparam1(v_bufs);
  CAMLlocal2(v_result, v_addr);
  int max_fds = Int_val(v_max_fds);
  int n_bufs = Wosize_val(v_bufs);
  struct iovec *iov;
  union sock_addr_union source_addr;
  int controllen = max_fds > 0 ? CMSG_SPACE(sizeof(int) * max_fds) : 0;
  char cmsg[controllen];
  struct msghdr msg = {
    .msg_name = &source_addr,
    .msg_namelen = sizeof(source_addr),
    .msg_iovlen = n_bufs,
    .msg_control = max_fds > 0 ? cmsg : NULL,
    .msg_controllen = controllen,
  };
  ssize_t r;

  memset(cmsg, 0, controllen);

  iov = alloc_iov(v_bufs);
  msg.msg_iov = iov;

  caml_enter_blocking_section();
  r = recvmsg(Int_val(v_fd), &msg, 0);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("recv_msg", Nothing);

  v_addr = safe_caml_unix_alloc_sockaddr(&source_addr, msg.msg_namelen, -1);

  v_result = caml_alloc_tuple(3);
  Store_field(v_result, 0, v_addr);
  Store_field(v_result, 1, Val_long(r));
  Store_field(v_result, 2, get_msghdr_fds(&msg));

  CAMLreturn(v_result);
}

CAMLprim value caml_eio_posix_fdopendir(value v_fd) {
  DIR *d = fdopendir(Int_val(v_fd));
  if (!d)
    caml_uerror("fdopendir", Nothing);

  value v_result = caml_alloc_small(1, Abstract_tag);
  DIR_Val(v_result) = d;
  return v_result;
}
