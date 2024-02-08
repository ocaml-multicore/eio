/*
 * Copyright (c) 2004 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */


#include <stdio.h>
#include <errno.h>
#include <paths.h>
#include <fcntl.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <limits.h>

#include <pty.h>
#include <utmp.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

value eio_unix_open_pty(value v_unit)
{
    CAMLparam1 (v_unit);
    char namebuf[4096]; /* Not PATH_MAX due to portability issues */
    int i, masterfd, slavefd;
    CAMLlocal1(v_ret);

    i = openpty(&masterfd, &slavefd, namebuf, NULL, NULL);
    if (i < 0)
        caml_uerror("openpty", Nothing);

    v_ret = caml_alloc_small(3, 0);
    Store_field(v_ret, 0, Val_int(masterfd));
    Store_field(v_ret, 1, Val_int(slavefd));
    Store_field(v_ret, 2, caml_copy_string(namebuf));
    CAMLreturn (v_ret);
}

value eio_unix_window_size(value pty, value pty_window)
{
    CAMLparam2 (pty, pty_window);
    int ptyfd;
    struct winsize w;
    w.ws_row = Int32_val(Field(pty_window, 0));
    w.ws_col = Int32_val(Field(pty_window, 1));
    w.ws_xpixel = Int32_val(Field(pty_window, 2));
    w.ws_ypixel = Int32_val(Field(pty_window, 3));
    ptyfd = Int_val(Field(pty, 0));
    ioctl(ptyfd, TIOCSWINSZ, &w);
    CAMLreturn (Val_unit);
}

value eio_unix_tty_window_size(value unit)
{
    CAMLparam1 (unit);
    CAMLlocal1(pty_window);

    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1)
        memset(&w, 0, sizeof(w));
    pty_window = caml_alloc_small(4, 0);
    Store_field(pty_window, 0, caml_copy_int32(w.ws_row));
    Store_field(pty_window, 1, caml_copy_int32(w.ws_col));
    Store_field(pty_window, 2, caml_copy_int32(w.ws_xpixel));
    Store_field(pty_window, 3, caml_copy_int32(w.ws_ypixel));
    CAMLreturn (pty_window);
}
