#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <winsock2.h>
#include <windows.h>
#include <assert.h>
#include <ntstatus.h>
#include <bcrypt.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

#ifdef ARCH_SIXTYFOUR
#define Int63_val(v) Long_val(v)
#else
#define Int63_val(v) (Int64_val(v)) >> 1
#endif

static void caml_stat_free_preserving_errno(void *ptr)
{
  int saved = errno;
  caml_stat_free(ptr);
  errno = saved;
}

CAMLprim value caml_eio_windows_getrandom(value v_ba, value v_off, value v_len)
{
  CAMLparam1(v_ba);
  ssize_t ret;
  ssize_t off = (ssize_t)Long_val(v_off);
  ssize_t len = (ssize_t)Long_val(v_len);
  do
  {
    void *buf = (uint8_t *)Caml_ba_data_val(v_ba) + off;
    caml_enter_blocking_section();
    ret = BCryptGenRandom(NULL, buf, len, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
    caml_leave_blocking_section();
  } while (errno == EINTR);
  if (ret != STATUS_SUCCESS)
    uerror("getrandom", Nothing);
  CAMLreturn(Val_long(len));
}

CAMLprim value caml_eio_windows_readv(value v_fd, value v_bufs)
{
  uerror("Readv is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_preadv(value v_fd, value v_bufs, value v_offset)
{
  uerror("Preadv is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_pwritev(value v_fd, value v_bufs, value v_offset)
{
  uerror("Pwritev is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_openat(value v_dirfd, value v_pathname, value v_flags, value v_mode)
{
  uerror("Readv is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_mkdirat(value v_fd, value v_path, value v_perm)
{
  uerror("mkdirat is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_unlinkat(value v_fd, value v_path, value v_dir)
{
  uerror("unlinkat is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path)
{
  uerror("renameat is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_spawn(value v_errors, value v_actions)
{
  uerror("Processes are not supported on windows yet", Nothing);
}
