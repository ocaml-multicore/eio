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

CAMLprim value caml_eio_windows_openat(value v_dirfd, value v_beneath, value v_open_link, value v_pathname, value v_desired_access, value v_create_disposition, value v_create_options)
{
  CAMLparam2(v_dirfd, v_pathname);
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;

  caml_unix_check_path(v_pathname, "openat");
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  dir = Is_some(v_dirfd) ? Handle_val(Some_val(v_dirfd)) : NULL;

  InitializeObjectAttributes(
    &obj_attr,
    &relative,
    OBJ_CASE_INSENSITIVE | (Bool_val(v_beneath) ? OBJ_DONT_REPARSE : 0),
    dir,
    NULL
  );

  r = NtCreateFile(
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
      | Int_val(v_create_options)
      | (Bool_val(v_open_link) ? FILE_OPEN_REPARSE_POINT : 0)),
    NULL, // Extended attribute buffer
    0     // Extended attribute buffer length
  );

  caml_stat_free(pathname);

  if (!NT_SUCCESS(r)) {
    DWORD err = RtlNtStatusToDosError(r);
    if (err == ERROR_REPARSE_POINT_ENCOUNTERED)
      caml_unix_error(-WSAELOOP, "openat", v_pathname);
    caml_win32_maperr(err);
    uerror("openat", v_pathname);
  }

  CAMLreturn(caml_win32_alloc_handle(h));
}

value caml_eio_windows_openat_bytes(value* values, int argc) {
    return caml_eio_windows_openat(values[0], values[1], values[2], values[3], values[4], values[5], values[6]);
}

CAMLprim value caml_eio_windows_reparse_point(value v_fd)
{
  CAMLparam1(v_fd);
  CAMLlocal2(target, result);
  HANDLE h = Handle_val(v_fd);
  BY_HANDLE_FILE_INFORMATION info;
  union {
    REPARSE_DATA_BUFFER point;
    char bytes[MAXIMUM_REPARSE_DATA_BUFFER_SIZE + sizeof(wchar_t)];
  } buffer;
  DWORD len;
  size_t header, offset, length;
  BOOL relative = FALSE;

  if (!GetFileInformationByHandle(h, &info) || !(info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT))
    CAMLreturn(Val_none);

  if (!DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, NULL, 0, buffer.bytes,
                       MAXIMUM_REPARSE_DATA_BUFFER_SIZE, &len, NULL)) {
    caml_win32_maperr(GetLastError());
    uerror("readlink", Nothing);
  }
  if (len < FIELD_OFFSET(REPARSE_DATA_BUFFER, GenericReparseBuffer.DataBuffer))
    caml_unix_error(EINVAL, "readlink", Nothing);

  switch (buffer.point.ReparseTag) {
  case IO_REPARSE_TAG_SYMLINK:
    header = FIELD_OFFSET(REPARSE_DATA_BUFFER, SymbolicLinkReparseBuffer.PathBuffer);
    if (len < header) caml_unix_error(EINVAL, "readlink", Nothing);
    offset = buffer.point.SymbolicLinkReparseBuffer.SubstituteNameOffset;
    length = buffer.point.SymbolicLinkReparseBuffer.SubstituteNameLength;
    relative = (buffer.point.SymbolicLinkReparseBuffer.Flags & SYMLINK_FLAG_RELATIVE) != 0;
    break;
  case IO_REPARSE_TAG_MOUNT_POINT:
    header = FIELD_OFFSET(REPARSE_DATA_BUFFER, MountPointReparseBuffer.PathBuffer);
    if (len < header) caml_unix_error(EINVAL, "readlink", Nothing);
    offset = buffer.point.MountPointReparseBuffer.SubstituteNameOffset;
    length = buffer.point.MountPointReparseBuffer.SubstituteNameLength;
    break;
  default:
    CAMLreturn(Val_none);
  }
  if (offset % sizeof(wchar_t) || length % sizeof(wchar_t) || header + offset + length > len)
    caml_unix_error(EINVAL, "readlink", Nothing);
  /* The counted name need not be terminated; the buffer has one spare WCHAR. */
  wchar_t *name = (wchar_t *)(buffer.bytes + header + offset);
  name[length / sizeof(wchar_t)] = 0;
  target = caml_copy_string_of_utf16(name);
  result = caml_alloc_small(3, 0);
  Field(result, 0) = target;
  Field(result, 1) = Val_bool(relative);
  Field(result, 2) = Val_long(length);
  CAMLreturn(caml_alloc_some(result));
}

/* GetFullPathNameW is lexical: the target need not exist and no links are followed. */
CAMLprim value caml_eio_windows_full_path(value v_path)
{
  CAMLparam1(v_path);
  wchar_t path[32768];
  caml_unix_check_path(v_path, "full_path");
  wchar_t *input = caml_stat_strdup_to_utf16(String_val(v_path));
  DWORD len = GetFullPathNameW(input, 32768, path, NULL);
  DWORD err = GetLastError();
  caml_stat_free(input);
  if (len == 0) {
    caml_win32_maperr(err);
    uerror("full_path", v_path);
  }
  if (len >= 32768) caml_unix_error(ENAMETOOLONG, "full_path", v_path);
  CAMLreturn(caml_copy_string_of_utf16(path));
}

CAMLprim value caml_eio_windows_realpath_fd(value v_fd)
{
  CAMLparam1(v_fd);
  wchar_t path[32768];
  HANDLE h = Handle_val(v_fd);
  DWORD len, err;
  caml_enter_blocking_section();
  len = GetFinalPathNameByHandleW(h, path, 32768, FILE_NAME_NORMALIZED | VOLUME_NAME_DOS);
  err = GetLastError();
  caml_leave_blocking_section();
  if (len == 0) {
    caml_win32_maperr(err);
    uerror("realpath", Nothing);
  }
  if (len >= 32768) caml_unix_error(ENAMETOOLONG, "realpath", Nothing);
  CAMLreturn(caml_copy_string_of_utf16(path));
}

CAMLprim value caml_eio_windows_readdir_batch(value v_fd, value v_restart)
{
  CAMLparam2(v_fd, v_restart);
  CAMLlocal3(result, name, cell);
  HANDLE h = Handle_val(v_fd);
  union {
    FILE_ID_BOTH_DIR_INFO align;
    char bytes[65536 + sizeof(wchar_t)];
  } buffer;
  FILE_INFO_BY_HANDLE_CLASS cls = Bool_val(v_restart) ? FileIdBothDirectoryRestartInfo : FileIdBothDirectoryInfo;
  caml_enter_blocking_section();
  BOOL ok = GetFileInformationByHandleEx(h, cls, buffer.bytes, 65536);
  DWORD err = GetLastError();
  caml_leave_blocking_section();
  if (!ok) {
    if (err == ERROR_NO_MORE_FILES || (Bool_val(v_restart) && err == ERROR_FILE_NOT_FOUND))
      CAMLreturn(Val_emptylist);
    caml_win32_maperr(err);
    uerror("readdir", Nothing);
  }
  result = Val_emptylist;
  FILE_ID_BOTH_DIR_INFO *entry = (FILE_ID_BOTH_DIR_INFO *)buffer.bytes;
  for (;;) {
    DWORD next = entry->NextEntryOffset;
    size_t len = entry->FileNameLength / sizeof(wchar_t);
    /* Leave room for a terminator even when the last name fills the buffer. */
    wchar_t saved = next ? entry->FileName[len] : 0;
    entry->FileName[len] = 0;
    name = caml_copy_string_of_utf16(entry->FileName);
    entry->FileName[len] = saved;
    cell = caml_alloc_small(2, 0);
    Field(cell, 0) = name;
    Field(cell, 1) = result;
    result = cell;
    if (next == 0) break;
    entry = (FILE_ID_BOTH_DIR_INFO *)((char *)entry + next);
  }
  CAMLreturn(result);
}

/* Privilege acquisition is thread-local; Developer Mode may allow creation without it. */
typedef NTSTATUS (NTAPI *pRtlAcquirePrivilege)(PULONG, ULONG, ULONG, PVOID *);
typedef VOID (NTAPI *pRtlReleasePrivilege)(PVOID);

CAMLprim value caml_eio_windows_symlinkat(value v_dir, value v_path, value v_to_dir, value v_data)
{
  CAMLparam4(v_dir, v_path, v_to_dir, v_data);
  union {
    REPARSE_DATA_BUFFER point;
    char bytes[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
  } buffer;
  size_t header = FIELD_OFFSET(REPARSE_DATA_BUFFER, SymbolicLinkReparseBuffer.PathBuffer);
  caml_unix_check_path(v_path, "symlink");
  caml_unix_check_path(Field(v_data, 0), "symlink");
  caml_unix_check_path(Field(v_data, 1), "symlink");
  wchar_t *target = caml_stat_strdup_to_utf16(String_val(Field(v_data, 0)));
  wchar_t *print = caml_stat_strdup_to_utf16(String_val(Field(v_data, 1)));
  size_t target_len = wcslen(target) * sizeof(wchar_t);
  size_t print_len = wcslen(print) * sizeof(wchar_t);
  size_t size = header + target_len + print_len;
  if (size > sizeof(buffer.bytes)) {
    caml_stat_free(target);
    caml_stat_free(print);
    caml_unix_error(ENAMETOOLONG, "symlink", v_path);
  }
  memset(&buffer, 0, header);
  buffer.point.ReparseTag = IO_REPARSE_TAG_SYMLINK;
  buffer.point.ReparseDataLength = (USHORT)(size - FIELD_OFFSET(REPARSE_DATA_BUFFER, GenericReparseBuffer.DataBuffer));
  buffer.point.SymbolicLinkReparseBuffer.SubstituteNameLength = (USHORT)target_len;
  buffer.point.SymbolicLinkReparseBuffer.PrintNameOffset = (USHORT)target_len;
  buffer.point.SymbolicLinkReparseBuffer.PrintNameLength = (USHORT)print_len;
  buffer.point.SymbolicLinkReparseBuffer.Flags = Bool_val(Field(v_data, 2)) ? SYMLINK_FLAG_RELATIVE : 0;
  memcpy(buffer.bytes + header, target, target_len);
  memcpy(buffer.bytes + header + target_len, print, print_len);
  caml_stat_free(target);
  caml_stat_free(print);
  HANDLE dir = Handle_val(v_dir), h;
  OBJECT_ATTRIBUTES attr;
  IO_STATUS_BLOCK io;
  UNICODE_STRING name;
  DWORD err = 0, returned;
  NTSTATUS status;
  PVOID privilege_state = NULL;
  ULONG privilege = 35;  /* SE_CREATE_SYMBOLIC_LINK_PRIVILEGE */
  HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
  pRtlAcquirePrivilege acquire = (pRtlAcquirePrivilege)GetProcAddress(ntdll, "RtlAcquirePrivilege");
  pRtlReleasePrivilege release = (pRtlReleasePrivilege)GetProcAddress(ntdll, "RtlReleasePrivilege");
  wchar_t *path = caml_stat_strdup_to_utf16(String_val(v_path));
  RtlInitUnicodeString(&name, path);
  InitializeObjectAttributes(&attr, &name, OBJ_CASE_INSENSITIVE, dir, NULL);
  ULONG options = FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT |
    (Bool_val(v_to_dir) ? FILE_DIRECTORY_FILE : FILE_NON_DIRECTORY_FILE);
  caml_enter_blocking_section();
  if (acquire && release) acquire(&privilege, 1, 0, &privilege_state);
  status = NtCreateFile(&h, SYNCHRONIZE | FILE_WRITE_ATTRIBUTES | DELETE, &attr, &io,
                  NULL, FILE_ATTRIBUTE_NORMAL, 0, FILE_CREATE, options, NULL, 0);
  if (!NT_SUCCESS(status)) err = RtlNtStatusToDosError(status);
  else {
    if (!DeviceIoControl(h, FSCTL_SET_REPARSE_POINT, buffer.bytes, (DWORD)size, NULL, 0, &returned, NULL)) {
      err = GetLastError();
      /* Roll back the object we created, using its handle rather than its name. */
      FILE_DISPOSITION_INFO disposition = { TRUE };
      SetFileInformationByHandle(h, FileDispositionInfo, &disposition, sizeof(disposition));
    }
    CloseHandle(h);
  }
  if (privilege_state) release(privilege_state);
  caml_leave_blocking_section();
  caml_stat_free(path);
  if (err) {
    caml_win32_maperr(err);
    uerror("symlink", v_path);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_windows_unlinkat(value v_dirfd, value v_pathname, value v_dir)
{
  CAMLparam3(v_dirfd, v_pathname, v_dir);
  HANDLE h, dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  wchar_t *pathname;
  UNICODE_STRING relative;
  NTSTATUS r;

  caml_unix_check_path(v_pathname, "unlinkat");
  pathname = caml_stat_strdup_to_utf16(String_val(v_pathname));
  RtlInitUnicodeString(&relative, pathname);

  dir = Is_some(v_dirfd) ? Handle_val(Some_val(v_dirfd)) : NULL;

  InitializeObjectAttributes(
    &obj_attr,
    &relative,
    OBJ_CASE_INSENSITIVE,
    dir,
    NULL
  );

  /* Ordinary files and rmdir need only DELETE access. Only directory links
     need an attribute query, to distinguish them from ordinary directories. */
  BOOL check_link = FALSE;
retry_open:
  r = NtCreateFile(
    &h,
    (SYNCHRONIZE | DELETE | (check_link ? FILE_READ_ATTRIBUTES : 0)),
    &obj_attr,
    &io_status,
    0, // Allocation size
    FILE_ATTRIBUTE_NORMAL, // TODO: Could check flags to see if we can do READONLY here a la OCaml
    (FILE_SHARE_DELETE),
    FILE_OPEN,
    (((Bool_val(v_dir) || check_link) ? FILE_DIRECTORY_FILE : FILE_NON_DIRECTORY_FILE) | FILE_SYNCHRONOUS_IO_NONALERT
     | FILE_OPEN_REPARSE_POINT),
    NULL, // Extended attribute buffer
    0     // Extended attribute buffer length
  );

  if (r == STATUS_FILE_IS_A_DIRECTORY && !Bool_val(v_dir) && !check_link) {
    check_link = TRUE;
    goto retry_open;
  }

  caml_stat_free(pathname);

  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("unlinkat", v_pathname);
  }

  /* Check and delete the same handle to prevent replacement between operations. */
  DWORD err = 0;
  if (check_link) {
    FILE_ATTRIBUTE_TAG_INFO info;
    if (!GetFileInformationByHandleEx(h, FileAttributeTagInfo, &info, sizeof(info)))
      err = GetLastError();
    else if (!(info.FileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) ||
             (info.ReparseTag != IO_REPARSE_TAG_SYMLINK && info.ReparseTag != IO_REPARSE_TAG_MOUNT_POINT))
      err = ERROR_ACCESS_DENIED;
  }
  FILE_DISPOSITION_INFO disposition = { TRUE };
  if (!err && !SetFileInformationByHandle(h, FileDispositionInfo, &disposition, sizeof(disposition)))
    err = GetLastError();
  CloseHandle(h);
  if (err) {
    caml_win32_maperr(err);
    uerror("unlinkat", v_pathname);
  }

  CAMLreturn(Val_unit);
}

/* Missing from mingw's winternl.h but fixed by Windows kernel ABI */
#define FileRenameInformationEx ((FILE_INFORMATION_CLASS)65)

CAMLprim value caml_eio_windows_renameat(value v_old_fd, value v_old_path, value v_new_fd, value v_new_path)
{
  CAMLparam4(v_old_fd, v_old_path, v_new_fd, v_new_path);
  HANDLE h, old_dir, new_dir;
  OBJECT_ATTRIBUTES obj_attr;
  IO_STATUS_BLOCK io_status;
  UNICODE_STRING relative;
  wchar_t *old_path, *new_path;
  FILE_RENAME_INFO *info;
  size_t name_len, info_size;
  NTSTATUS r;

  caml_unix_check_path(v_old_path, "renameat");
  caml_unix_check_path(v_new_path, "renameat");
  old_dir = Is_some(v_old_fd) ? Handle_val(Some_val(v_old_fd)) : NULL;
  new_dir = Is_some(v_new_fd) ? Handle_val(Some_val(v_new_fd)) : NULL;

  old_path = caml_stat_strdup_to_utf16(String_val(v_old_path));
  RtlInitUnicodeString(&relative, old_path);
  InitializeObjectAttributes(&obj_attr, &relative, OBJ_CASE_INSENSITIVE, old_dir, NULL);
  caml_enter_blocking_section();
  r = NtCreateFile(
    &h,
    DELETE | SYNCHRONIZE,
    &obj_attr,
    &io_status,
    0,
    FILE_ATTRIBUTE_NORMAL,
    (FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE),
    FILE_OPEN,
    (FILE_SYNCHRONOUS_IO_NONALERT | FILE_OPEN_FOR_BACKUP_INTENT | FILE_OPEN_REPARSE_POINT),
    NULL,
    0
  );
  caml_leave_blocking_section();
  caml_stat_free(old_path);
  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("renameat", v_old_path);
  }

  new_path = caml_stat_strdup_to_utf16(String_val(v_new_path));
  name_len = wcslen(new_path) * sizeof(wchar_t);
  info_size = sizeof(FILE_RENAME_INFO) + name_len;
  info = caml_stat_alloc(info_size);
  info->Flags = FILE_RENAME_FLAG_REPLACE_IF_EXISTS | FILE_RENAME_FLAG_POSIX_SEMANTICS;
  info->RootDirectory = new_dir;
  info->FileNameLength = (ULONG)name_len;
  memcpy(info->FileName, new_path, name_len);
  caml_stat_free(new_path);

  caml_enter_blocking_section();
  r = NtSetInformationFile(h, &io_status, info, (ULONG)info_size, FileRenameInformationEx);
  if (r == STATUS_INVALID_PARAMETER || r == STATUS_INVALID_INFO_CLASS) {
    /* FAT32 volumes only seem to have the original class afaict */
    info->Flags = 0;
    info->ReplaceIfExists = TRUE;
    r = NtSetInformationFile(h, &io_status, info, (ULONG)info_size, FileRenameInformation);
  }
  CloseHandle(h);
  caml_leave_blocking_section();
  caml_stat_free(info);
  if (!NT_SUCCESS(r)) {
    caml_win32_maperr(RtlNtStatusToDosError(r));
    uerror("renameat", v_new_path);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_eio_windows_spawn(value v_cwd, value v_env,
                                      value v_stdin, value v_stdout, value v_stderr,
                                      value v_cmdline)
{
  CAMLparam5(v_cwd, v_env, v_stdin, v_stdout, v_stderr);
  CAMLxparam1(v_cmdline);
  CAMLlocal1(v_result);

  wchar_t *cmdline = NULL, *cwd = NULL, *env_block = NULL;
  HANDLE src[3];
  HANDLE dup[3] = { NULL, NULL, NULL };
  STARTUPINFOEXW si;
  PROCESS_INFORMATION pi;
  SIZE_T attr_size = 0;
  BOOL ok = FALSE;
  DWORD err = 0;
  DWORD create_flags = EXTENDED_STARTUPINFO_PRESENT | CREATE_UNICODE_ENVIRONMENT;
  HANDLE cur = GetCurrentProcess();

  memset(&si, 0, sizeof(si));
  memset(&pi, 0, sizeof(pi));

  caml_unix_check_path(v_cmdline, "execve");
  if (Is_some(v_cwd)) caml_unix_check_path(Field(v_cwd, 0), "execve");
  cmdline = caml_stat_strdup_to_utf16(String_val(v_cmdline));
  if (Is_some(v_cwd)) cwd = caml_stat_strdup_to_utf16(String_val(Field(v_cwd, 0)));
  env_block = caml_stat_char_array_to_utf16(String_val(v_env), caml_string_length(v_env), NULL);

  src[0] = Handle_val(v_stdin);
  src[1] = Handle_val(v_stdout);
  src[2] = Handle_val(v_stderr);

  for (int i = 0; i < 3; i++) {
    if (!DuplicateHandle(cur, src[i], cur, &dup[i], 0, TRUE, DUPLICATE_SAME_ACCESS)) {
      err = GetLastError();
      goto cleanup;
    }
  }

  si.StartupInfo.cb = sizeof(STARTUPINFOEXW);
  si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  si.StartupInfo.hStdInput  = dup[0];
  si.StartupInfo.hStdOutput = dup[1];
  si.StartupInfo.hStdError  = dup[2];

  if (!GetConsoleWindow()) create_flags |= CREATE_NO_WINDOW;

  InitializeProcThreadAttributeList(NULL, 1, 0, &attr_size);
  si.lpAttributeList = caml_stat_alloc(attr_size);
  if (!InitializeProcThreadAttributeList(si.lpAttributeList, 1, 0, &attr_size)) {
    err = GetLastError();
    caml_stat_free(si.lpAttributeList);
    si.lpAttributeList = NULL;
    goto cleanup;
  }
  if (!UpdateProcThreadAttribute(si.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                 dup, 3 * sizeof(HANDLE), NULL, NULL)) {
    err = GetLastError();
    goto cleanup;
  }

  caml_enter_blocking_section();
  ok = CreateProcessW(NULL, cmdline, NULL, NULL, TRUE, create_flags, env_block, cwd,
                      &si.StartupInfo, &pi);
  if (!ok) err = GetLastError();
  caml_leave_blocking_section();

cleanup:
  if (si.lpAttributeList) {
    DeleteProcThreadAttributeList(si.lpAttributeList);
    caml_stat_free(si.lpAttributeList);
  }
  for (int i = 0; i < 3; i++)
    if (dup[i]) CloseHandle(dup[i]);
  caml_stat_free(cmdline);
  caml_stat_free(cwd);
  caml_stat_free(env_block);

  if (!ok) {
    caml_win32_maperr(err);
    /* Named "execve" so the portable error translation applies unchanged. */
    uerror("execve", v_cmdline);
  }

  CloseHandle(pi.hThread);
  v_result = caml_alloc_tuple(2);
  Store_field(v_result, 0, Val_long(pi.dwProcessId));
  Store_field(v_result, 1, caml_win32_alloc_handle(pi.hProcess));
  CAMLreturn(v_result);
}

CAMLprim value caml_eio_windows_spawn_bytes(value *argv, int argn)
{
  (void)argn;
  return caml_eio_windows_spawn(argv[0], argv[1], argv[2], argv[3],
                                argv[4], argv[5]);
}

CAMLprim value caml_eio_windows_process_wait(value v_handle)
{
  CAMLparam1(v_handle);
  HANDLE h = Handle_val(v_handle);
  DWORD code = 0;
  DWORD wait_res;
  caml_enter_blocking_section();
  wait_res = WaitForSingleObject(h, INFINITE);
  caml_leave_blocking_section();
  if (wait_res == WAIT_FAILED) {
    caml_win32_maperr(GetLastError());
    uerror("process_wait", Nothing);
  }
  if (!GetExitCodeProcess(h, &code)) {
    caml_win32_maperr(GetLastError());
    uerror("process_wait", Nothing);
  }
  CAMLreturn(Val_long((intnat)(unsigned int)code));
}

CAMLprim value caml_eio_windows_process_terminate(value v_handle, value v_code)
{
  CAMLparam2(v_handle, v_code);
  /* Signalling an already-exited process is a no-op rather than an error. */
  CAMLreturn(Val_bool(TerminateProcess(Handle_val(v_handle), (UINT)Long_val(v_code))));
}
