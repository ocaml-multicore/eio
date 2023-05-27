/* From mirage/ocaml-cstruct
Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
Copyright (c) 2012 Pierre Chambart
Copyright (c) Christiano F. Haesbaert <haesbaert@haesbaert.org>
Copyright (c) Citrix Inc
Copyright (c) David Sheets <sheets@alum.mit.edu>
Copyright (c) Drup <drupyog@zoho.com>
Copyright (c) Hannes Mehnert <hannes@mehnert.org>
Copyright (c) Jeremy Yallop <yallop@gmail.com>
Copyright (c) Mindy Preston <meetup@yomimono.org>
Copyright (c) Nicolas Ojeda Bar <n.oje.bar@gmail.com>
Copyright (c) Richard Mortier <mort@cantab.net>
Copyright (c) Rudi Grinberg <rudi.grinberg@gmail.com>
Copyright (c) Thomas Gazagnaire <thomas@gazagnaire.com>
Copyright (c) Thomas Leonard <talex5@gmail.com>
Copyright (c) Vincent Bernardoff <vb@luminar.eu.org>
Copyright (c) pqwy <david@numm.org>

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/fail.h>

#include <stdio.h>
#include <errno.h>

CAMLprim value eio_windows_cstruct_read(value val_fd, value val_c)
{
  CAMLparam2(val_fd, val_c);
  CAMLlocal3(val_buf, val_ofs, val_len);
  uint8_t *buf;
  size_t len;
  ssize_t n = 0;
  int win32err = 0;
  SOCKET s;
  HANDLE h;
  DWORD numread;
  int ok;

  val_buf = Field(val_c, 0);
  val_ofs = Field(val_c, 1);
  val_len = Field(val_c, 2);

  buf = (uint8_t *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  len = (size_t)Long_val(val_len);

  switch (Descr_kind_val(val_fd))
  {
  case KIND_SOCKET:
    s = Socket_val(val_fd);

    caml_release_runtime_system();
    n = recv(s, buf, len, 0);
    win32err = WSAGetLastError();
    caml_acquire_runtime_system();

    if (n == SOCKET_ERROR)
    {
      win32_maperr(win32err);
      uerror("stub_cstruct_read", Nothing);
    }
    break;
  case KIND_HANDLE:
    h = Handle_val(val_fd);
    caml_release_runtime_system();
    ok = ReadFile(h, buf, len, &numread, NULL);
    win32err = GetLastError();
    n = numread;
    caml_acquire_runtime_system();

    if (!ok)
    {
      win32_maperr(win32err);
      uerror("stub_cstruct_read", Nothing);
    }
    break;
  default:
    caml_failwith("unknown Descr_kind_val");
  }

  CAMLreturn(Val_int(n));
}

CAMLprim value eio_windows_cstruct_write(value val_fd, value val_c)
{
  CAMLparam2(val_fd, val_c);
  CAMLlocal3(val_buf, val_ofs, val_len);
  val_buf = Field(val_c, 0);
  val_ofs = Field(val_c, 1);
  val_len = Field(val_c, 2);
  void *buf = (char *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  size_t len = Long_val(val_len);
  ssize_t n = 0;

  int win32err = 0;
  switch (Descr_kind_val(val_fd))
  {
  case KIND_SOCKET:
    SOCKET s = Socket_val(val_fd);

    caml_release_runtime_system();
    n = send(s, buf, len, 0);
    win32err = WSAGetLastError();
    caml_acquire_runtime_system();

    if (n == SOCKET_ERROR)
    {
      win32_maperr(win32err);
      unix_error(errno, "stub_cstruct_write", Nothing);
    }
    break;
  case KIND_HANDLE:
    HANDLE h = Handle_val(val_fd);
    DWORD numwritten;
    caml_release_runtime_system();
    int ok = WriteFile(h, buf, len, &numwritten, NULL);
    win32err = GetLastError();

    n = numwritten;
    caml_acquire_runtime_system();

    if (!ok)
    {
      win32_maperr(win32err);
      uerror("stub_cstruct_write", Nothing);
    }
    break;
  default:
    caml_failwith("unknown Descr_kind_val");
  }

  CAMLreturn(Val_int(n));
}