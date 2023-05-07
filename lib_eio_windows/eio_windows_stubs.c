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
#include <winternl.h>
#include <ntdef.h>

typedef ULONG (__stdcall *pNtCreateFile)(
   PHANDLE FileHandle,
   ULONG DesiredAccess,
   PVOID ObjectAttributes,
   PVOID IoStatusBlock,
   PLARGE_INTEGER AllocationSize,
   ULONG FileAttributes,
   ULONG ShareAccess,
   ULONG CreateDisposition,
   ULONG CreateOptions,
   PVOID EaBuffer,
   ULONG EaLength
 );

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/osdeps.h>

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
  uerror("readv is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_preadv(value v_fd, value v_bufs, value v_offset)
{
  uerror("preadv is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_pwritev(value v_fd, value v_bufs, value v_offset)
{
  uerror("pwritev is not supported on windows yet", Nothing);
}

// File-system operations

// No follow
void no_follow(HANDLE h) {
  BY_HANDLE_FILE_INFORMATION b;

  if (!GetFileInformationByHandle(h, &b)) {
    caml_win32_maperr(GetLastError());
    uerror("nofollow", Nothing);
  }

  if (b.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) {
    CloseHandle(h);
    caml_unix_error(ELOOP, "nofollow", Nothing);
  }
}

// We recreate an openat like function using NtCreateFile
CAMLprim value caml_eio_windows_openat(value v_dirfd, value v_nofollow, value v_pathname, value v_desired_access, value v_create_disposition, value v_create_options)
{
  CAMLparam2(v_dirfd, v_pathname);
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;

  // Not sure what the overhead of this is, but it allows us to have low-level control
  // over file creation. In particular, we can specify the HANDLE to the parent directory
  // of a relative path a la openat.
  pNtCreateFile NtCreatefile = (pNtCreateFile)GetProcAddress(GetModuleHandle("ntdll.dll"), "NtCreateFile");
  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  // If NULL the filepath has to be absolute
  if (Is_some(v_dirfd)) {
    dir = Handle_val(Field(v_dirfd, 0));
  } else {
    dir = NULL;
  }

  // Initialise object attributes, passing in the root directory FD
  InitializeObjectAttributes(
    &obj_attr,
    &relative,
    OBJ_CASE_INSENSITIVE, // TODO: Double-check what flags need to be passed at this point.
    dir,
    NULL
  );

  // Create the file
  r = NtCreatefile(
    &h,
    Int_val(v_desired_access) | FILE_READ_ATTRIBUTES,
    &obj_attr,
    &io_status,
    0, // Allocation size
    FILE_ATTRIBUTE_NORMAL, // TODO: Could check flags to see if we can do READONLY here a la OCaml
    (FILE_SHARE_READ | FILE_SHARE_WRITE),
    Int_val(v_create_disposition),
    ( 
       FILE_SYNCHRONOUS_IO_NONALERT
      | FILE_OPEN_FOR_BACKUP_INTENT
      | (Bool_val(v_nofollow) ? FILE_FLAG_OPEN_REPARSE_POINT : Int_val(v_create_options))),
    NULL, // Extended attribute buffer
    0     // Extended attribute buffer length
  );

  // Free the allocated pathname
  caml_stat_free(pathname);

  if (h == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("openat handle", v_pathname);
  }

   if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("openat", Nothing);
  }

  // No follow check -- Windows doesn't actually have that ability
  // so we have to do it after the fact. This will raise if a symbolic
  // link is encountered and will close the handle.
  if (Bool_val(v_nofollow)) {
    no_follow(h);
  }
  
  CAMLreturn(caml_win32_alloc_handle(h));
}

value caml_eio_windows_openat_bytes(value* values, int argc) {
    return caml_eio_windows_openat(values[0], values[1], values[2], values[3], values[4], values[5]);
}

CAMLprim value caml_eio_windows_unlinkat(value v_dirfd, value v_pathname, value v_dir)
{
  CAMLparam2(v_dirfd, v_pathname);
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;

  // Not sure what the overhead of this is, but it allows us to have low-level control
  // over file creation. In particular, we can specify the HANDLE to the parent directory
  // of a relative path a la openat.
  pNtCreateFile NtCreatefile = (pNtCreateFile)GetProcAddress(GetModuleHandle("ntdll.dll"), "NtCreateFile");
  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  // If NULL the filepath has to be absolute
  if (Is_some(v_dirfd)) {
    dir = Handle_val(Field(v_dirfd, 0));
  } else {
    dir = NULL;
  }

  // Initialise object attributes, passing in the root directory FD
  InitializeObjectAttributes(
    &obj_attr,
    &relative,
    OBJ_CASE_INSENSITIVE, // TODO: Double-check what flags need to be passed at this point.
    dir,
    NULL
  );

  // Create the file
  r = NtCreatefile(
    &h,
    (SYNCHRONIZE | DELETE),
    &obj_attr,
    &io_status,
    0, // Allocation size
    FILE_ATTRIBUTE_NORMAL, // TODO: Could check flags to see if we can do READONLY here a la OCaml
    (FILE_SHARE_DELETE),
    FILE_OPEN,
    ((Bool_val(v_dir) ? FILE_DIRECTORY_FILE : FILE_NON_DIRECTORY_FILE) | FILE_SYNCHRONOUS_IO_NONALERT | FILE_DELETE_ON_CLOSE),
    NULL, // Extended attribute buffer
    0     // Extended attribute buffer length
  );

  // Free the allocated pathname
  caml_stat_free(pathname);

  if (h == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("openat", v_pathname);
  }

  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("openat", Nothing);
  }

  // Now close the file to delete it
  BOOL closed;
  closed = CloseHandle(h);
  
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_windows_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path)
{
  uerror("renameat is not supported on windows yet", Nothing);
}

CAMLprim value caml_eio_windows_spawn(value v_errors, value v_actions)
{
  uerror("processes are not supported on windows yet", Nothing);
}
