/* Change an existing directory into a junction without renaming it. This lets
   the tests reproduce a concurrent reparse-point change deterministically. */
#include <winsock2.h>
#include <windows.h>
#include <winioctl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>

CAMLprim value caml_test_set_junction(value v_path, value v_target)
{
  CAMLparam2(v_path, v_target);
  struct {
    DWORD tag;
    WORD length, reserved;
    WORD sub_offset, sub_length, print_offset, print_length;
    WCHAR path[4096];
  } data = {0};
  DWORD returned, err = 0;
  caml_unix_check_path(v_path, "test-junction");
  wchar_t *path = caml_stat_strdup_to_utf16(String_val(v_path));
  HANDLE h = CreateFileW(path, FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ | FILE_SHARE_WRITE,
                        NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
  caml_stat_free(path);
  if (h == INVALID_HANDLE_VALUE) {
    caml_win32_maperr(GetLastError());
    uerror("test-junction", v_path);
  }
  data.tag = IO_REPARSE_TAG_MOUNT_POINT;
  DWORD code = FSCTL_DELETE_REPARSE_POINT;
  if (Is_some(v_target)) {
    wchar_t *target = caml_stat_strdup_to_utf16(String_val(Some_val(v_target)));
    size_t len = wcslen(target);
    if (len + 6 >= 4096) {
      caml_stat_free(target);
      CloseHandle(h);
      caml_unix_error(ENAMETOOLONG, "test-junction", v_path);
    }
    wcscpy(data.path, L"\\??\\");
    wcscat(data.path, target);
    caml_stat_free(target);
    data.sub_length = (WORD)((len + 4) * sizeof(WCHAR));
    data.print_offset = data.sub_length + sizeof(WCHAR);
    data.length = 8 + data.print_offset + sizeof(WCHAR);
    code = FSCTL_SET_REPARSE_POINT;
  }
  if (!DeviceIoControl(h, code, &data, data.length + 8, NULL, 0, &returned, NULL)) err = GetLastError();
  CloseHandle(h);
  if (err) {
    caml_win32_maperr(err);
    uerror("test-junction", v_path);
  }
  CAMLreturn(Val_unit);
}
