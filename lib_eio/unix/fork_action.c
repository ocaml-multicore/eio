/* Note: fork actions MUST NOT allocate (either on the OCaml heap or with C malloc).
 * This is because e.g. we might have forked while another thread in the parent had a lock.
 * In the child, we inherit a copy of the locked mutex, but no corresponding thread to
 * release it.
 */

#include "primitives.h"

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "fork_action.h"

#ifndef _WIN32
void eio_unix_run_fork_actions(int errors, value v_actions) {
  int old_flags = fcntl(errors, F_GETFL, 0);
  fcntl(errors, F_SETFL, old_flags & ~O_NONBLOCK);
  while (Is_block(v_actions)) {
    value v_action = Field(v_actions, 0);
    fork_fn *action = (fork_fn *) Nativeint_val(Field(v_action, 0));
    action(errors, v_action);
    v_actions = Field(v_actions, 1);
  }
  _exit(1);
}
#endif

static void try_write_all(int fd, char *buf) {
  int len = strlen(buf);
  while (len > 0) {
    int wrote = write(fd, buf, len);

    if (wrote <= 0)
      return;

    buf += wrote;
    len -= wrote;
  }
}

void eio_unix_fork_error(int fd, char *fn, char *buf) {
  try_write_all(fd, fn);
  try_write_all(fd, ": ");
  try_write_all(fd, buf);
}

#define String_array_val(v) *((char ***)Data_custom_val(v))

static void finalize_string_array(value v) {
  free(String_array_val(v));
  String_array_val(v) = NULL;
}

static struct custom_operations string_array_ops = {
  "string.array",
  finalize_string_array,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value eio_unix_make_string_array(value v_len) {
  CAMLparam0();
  CAMLlocal1(v_str_array);
  int n = Int_val(v_len);
  uintnat total;

  if (caml_umul_overflow(sizeof(char *), n + 1, &total))
    caml_raise_out_of_memory();

  v_str_array = caml_alloc_custom_mem(&string_array_ops, sizeof(char ***), total);

  char **c = calloc(sizeof(char *), n + 1);
  String_array_val(v_str_array) = c;
  if (!c)
    caml_raise_out_of_memory();

  CAMLreturn(v_str_array);
}

static void fill_string_array(char **c, value v_ocaml_array) {
  int n = Wosize_val(v_ocaml_array);

  for (int i = 0; i < n; i++) {
    c[i] = (char *) String_val(Field(v_ocaml_array, i));
  }

  c[n] = NULL;
}

static void action_execve(int errors, value v_config) {
  value v_exe = Field(v_config, 1);
  char **argv = String_array_val(Field(v_config, 2));
  char **envp = String_array_val(Field(v_config, 4));

  fill_string_array(argv, Field(v_config, 3));
  fill_string_array(envp, Field(v_config, 5));

  execve(String_val(v_exe), argv, envp);
  eio_unix_fork_error(errors, "execve", strerror(errno));
  _exit(1);
}

CAMLprim value eio_unix_fork_execve(value v_unit) {
  return Val_fork_fn(action_execve);
}

static void action_fchdir(int errors, value v_config) {
  #ifdef _WIN32
  eio_unix_fork_error(errors, "action_fchdir", "Unsupported operation on windows");
  #else
  value v_fd = Field(v_config, 1);
  int r;
  r = fchdir(Int_val(v_fd));
  if (r != 0) {
    eio_unix_fork_error(errors, "fchdir", strerror(errno));
    _exit(1);
  }
  #endif
}

CAMLprim value eio_unix_fork_fchdir(value v_unit) {
  return Val_fork_fn(action_fchdir);
}

static void action_chdir(int errors, value v_config) {
  value v_path = Field(v_config, 1);
  int r;
  r = chdir(String_val(v_path));
  if (r != 0) {
    eio_unix_fork_error(errors, "chdir", strerror(errno));
    _exit(1);
  }
}

CAMLprim value eio_unix_fork_chdir(value v_unit) {
  return Val_fork_fn(action_chdir);
}

static void set_blocking(int errors, int fd, int blocking) {
  #ifdef _WIN32
  eio_unix_fork_error(errors, "set_blocking", "Unsupported operation on windows");
  #else
  int r = fcntl(fd, F_GETFL, 0);
  if (r != -1) {
    int flags = blocking
      ? r & ~O_NONBLOCK
      : r | O_NONBLOCK;
    if (r != flags) {
      r = fcntl(fd, F_SETFL, flags);
    }
  }
  if (r == -1) {
    eio_unix_fork_error(errors, "fcntl", strerror(errno));
    _exit(1);
  }
  #endif
}

static void set_cloexec(int errors, int fd, int cloexec) {
  #ifdef _WIN32
  eio_unix_fork_error(errors, "set_cloexec", "Unsupported operation on windows");
  #else
  int r = fcntl(fd, F_GETFD, 0);
  if (r != -1) {
    int flags = cloexec
      ? r | FD_CLOEXEC
      : r & ~FD_CLOEXEC;
    if (r != flags) {
      r = fcntl(fd, F_SETFD, flags);
    }
  }
  if (r == -1) {
    eio_unix_fork_error(errors, "fcntl", strerror(errno));
    _exit(1);
  }
  #endif
}

static void action_dups(int errors, value v_config) {
  value v_plan = Field(v_config, 1);
  value v_blocking = Field(v_config, 2);
  int tmp = -1;
  while (Is_block(v_plan)) {
    value v_dup = Field(v_plan, 0);
    int src = Int_val(Field(v_dup, 0));
    int dst = Int_val(Field(v_dup, 1));
    if (src == -1) src = tmp;
    if (dst == -1) {
      // Dup to a temporary FD
      if (tmp == -1) {
	tmp = dup(src);
	if (tmp < 0) {
	  eio_unix_fork_error(errors, "dup-tmp", strerror(errno));
	  _exit(1);
	}
      } else {
	int r = dup2(src, tmp);
	if (r < 0) {
	  eio_unix_fork_error(errors, "dup2-tmp", strerror(errno));
	  _exit(1);
	}
      }
      set_cloexec(errors, tmp, 1);
    } else if (src == dst) {
      set_cloexec(errors, dst, 0);
    } else {
      int r = dup2(src, dst);
      if (r < 0) {
	eio_unix_fork_error(errors, "dup2", strerror(errno));
	_exit(1);
      }
    }
    v_plan = Field(v_plan, 1);
  }
  while (Is_block(v_blocking)) {
    value v_flags = Field(v_blocking, 0);
    int fd = Int_val(Field(v_flags, 0));
    int blocking = Bool_val(Field(v_flags, 1));
    set_blocking(errors, fd, blocking);
    v_blocking = Field(v_blocking, 1);
  }
}

CAMLprim value eio_unix_fork_dups(value v_unit) {
  return Val_fork_fn(action_dups);
}
