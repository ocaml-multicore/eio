#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include <caml/mlvalues.h>

#include "fork_action.h"

void eio_unix_run_fork_actions(int errors, value v_actions) {
  int old_flags = fcntl(errors, F_GETFL, 0);
  fcntl(errors, F_SETFL, old_flags & ~O_NONBLOCK);
  while (Is_block(v_actions)) {
    value v_action = Field(v_actions, 0);
    fork_fn *action = (fork_fn *) Nativeint_val(Field(v_action, 0));
    int err = action(errors, v_action);
    if (err) {
      _exit(err);
    }
    v_actions = Field(v_actions, 1);
  }
  _exit(1);
}

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

static char **make_string_array(int errors, value v_array) {
  int n = Wosize_val(v_array);
  char **c = calloc(sizeof(char *), (n + 1));
  if (!c) {
    eio_unix_fork_error(errors, "make_string_array", "out of memory");
    _exit(1);
  }
  for (int i = 0; i < n; i++) {
    c[i] = (char *) String_val(Field(v_array, i));
  }
  c[n] = NULL;
  return c;
}

static int action_execve(int errors, value v_config) {
  value v_exe = Field(v_config, 1);
  char **argv = make_string_array(errors, Field(v_config, 2));
  char **envp = make_string_array(errors, Field(v_config, 3));
  execve(String_val(v_exe), argv, envp);
  eio_unix_fork_error(errors, "execve", strerror(errno));
  return 1;
}

CAMLprim value eio_unix_fork_execve(value v_unit) {
  return Val_fork_fn(action_execve);
}

static int action_fchdir(int errors, value v_config) {
  value v_fd = Field(v_config, 1);
  int r;
  r = fchdir(Int_val(v_fd));
  if (r != 0) {
    eio_unix_fork_error(errors, "fchdir", strerror(errno));
    return 1;
  }
  return 0;
}

CAMLprim value eio_unix_fork_fchdir(value v_unit) {
  return Val_fork_fn(action_fchdir);
}

static int action_chdir(int errors, value v_config) {
  value v_path = Field(v_config, 1);
  int r;
  r = chdir(String_val(v_path));
  if (r != 0) {
    eio_unix_fork_error(errors, "chdir", strerror(errno));
    return 1;
  }
  return 0;
}

CAMLprim value eio_unix_fork_chdir(value v_unit) {
  return Val_fork_fn(action_chdir);
}
