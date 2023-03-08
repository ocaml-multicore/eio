/*
The MIT License

Copyright (c) 2016-2018 Jane Street Group, LLC <opensource@janestreet.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#define _GNU_SOURCE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/fail.h>

#include <errno.h>

#define CAML_INTERNALS
/* for [caml_convert_signal_number] */
#include <caml/signals.h>
CAMLextern int caml_convert_signal_number(int);
#undef CAML_INTERNALS

#include <assert.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>

/* +-----------------------------------------------------------------+
   | pipe2                                                           |
   +-----------------------------------------------------------------+ */

#define enter_safe_pipe_section()
#define leave_safe_pipe_section()

static int safe_pipe(int fd[2])
{
  return pipe2(fd, O_CLOEXEC);
}

CAMLprim value spawn_pipe()
{
  int fd[2];
  value res;

  if (safe_pipe(fd) == -1) uerror("safe_pipe", Nothing);

  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);

  return res;
}

/* +-----------------------------------------------------------------+
   | Code executed in the child                                      |
   +-----------------------------------------------------------------+ */

enum error_arg { NOTHING, CWD, PROG };

/* Structure used to communicate errors from the child to the
   parent. */
struct subprocess_failure {
  /* Value of [errno]. */
  int error;
  /* System call that failed */
  char function[32];
  /* What to pass as third argument of the Unix_error exception. */
  enum error_arg arg;
};

/* Compile time asserts as described here:

   http://stackoverflow.com/questions/807244/c-compiler-asserts-how-to-implement
*/
#define CASSERT(predicate) _impl_CASSERT_LINE(predicate,__LINE__,__FILE__)
#define _impl_PASTE(a,b) a##b
#define _impl_CASSERT_LINE(predicate, line, file)                       \
  typedef char __attribute__((unused))                                  \
  _impl_PASTE(assertion_failed_##file##_,line)[2*!!(predicate)-1];

/* Fill a [subprocess_failure] structure. */
static void set_error(struct subprocess_failure *fp,
                      int error,
                      char *function,
                      enum error_arg error_arg)
{
  size_t len = strlen(function);
  assert(len + 1 <= sizeof(fp->function));
  memset(fp, 0, sizeof(*fp));
  memcpy(fp->function, function, len + 1);
  fp->error = error;
  fp->arg   = error_arg;
}

/* Report an error to the parent. Use the current value of [errno] as
   error number. */
static void subprocess_failure(int failure_fd,
                               char *function,
                               enum error_arg error_arg)
{
  struct subprocess_failure failure;
  sigset_t sigset;
  ssize_t written;

  CASSERT(sizeof(failure) < PIPE_BUF)

  set_error(&failure, errno, function, error_arg);

  /* Block all signals to avoid being interrupted in write.
     Although most of the call sites of [subprocess_failure] already block
     signals, the one after the [exec] might not. */
  sigfillset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, NULL);

  /* Write is atomic as buffer is smaller than PIPE_BUF
     (required by POSIX.1-2001, as claimed in [man 7 pipe]).

     We only store the result of [write] to avoid a warning.
 */
  written = write(failure_fd, &failure, sizeof(failure));
  (void)written;
  _exit(127);
}

/* same as [dup] but ensures the result is >= 3. */
static int safe_dup(int failure_fd, int fd)
{
  int new_fd = dup(fd);
  if (new_fd == -1) subprocess_failure(failure_fd, "dup", NOTHING);
  if (new_fd >= 3)
    return new_fd;
  else {
    int result = safe_dup(failure_fd, fd);
    close(new_fd);
    return result;
  }
}

enum working_dir_kind { PATH, FD };

struct spawn_info {
  char **env; /* can be 0, in which case the current environment is used */
  enum working_dir_kind cwd_kind;
  union {
    int fd;
    char *path;
  } cwd;
  char *prog;
  char **argv;
  int std_fds[3];
  int set_pgid;
  pid_t pgid;
  sigset_t child_sigmask;
};

static void subprocess(int failure_fd, struct spawn_info *info)
{
  int i, fd, tmp_fds[3];
  struct sigaction sa;

  if (info->set_pgid) {
    if (setpgid(0, info->pgid) == -1) {
      subprocess_failure(failure_fd, "setpgid", NOTHING);
      return;
    }
  }

  /* Restore all signals to their default behavior before setting the
     desired signal mask for the subprocess to avoid invoking handlers
     from the parent */
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  /* Ignore errors as there is no interesting way it can fail. */
  for (i = 1; i < NSIG; i++) sigaction(i, &sa, NULL);

  switch (info->cwd_kind) {
    case PATH:
      if (chdir(info->cwd.path) == -1)
        subprocess_failure(failure_fd, "chdir", CWD);
      break;
    case FD:
      if (fchdir(info->cwd.fd) == -1)
        subprocess_failure(failure_fd, "fchdir", NOTHING);
      close(info->cwd.fd);
      break;
  }

  /* Use temporary file descriptors for redirections to avoid problems
     when redirecting stdout to stderr for instance. */

  for (fd = 0; fd < 3; fd++)
    tmp_fds[fd] = safe_dup(failure_fd, info->std_fds[fd]);

  for (fd = 0; fd < 3; fd++)
    close(info->std_fds[fd]);

  for (fd = 0; fd < 3; fd++) {
    /* here we rely on [dup2] clearing the O_CLOEXEC flag */
    if (dup2(tmp_fds[fd], fd) == -1)
      subprocess_failure(failure_fd, "dup2", NOTHING);
    close(tmp_fds[fd]);
  }

  pthread_sigmask(SIG_SETMASK, &info->child_sigmask, NULL);

  execve(info->prog, info->argv, info->env);
  subprocess_failure(failure_fd, "execve", PROG);
}

/* +-----------------------------------------------------------------+
   | Parent code                                                     |
   +-----------------------------------------------------------------+ */

/* Convert a [string list] into a NULL terminated array of C
   strings.

   We don't reuse the [cstringvect] function from [unix_support.h] as
   it doesn't copy the strings in the array.
*/
static char **alloc_string_vect(value v)
{
  char **result;
  mlsize_t count, i, full_size;
  value x;
  char *ptr;

  count = 0;
  full_size = sizeof(char*);
  for (x = v; Is_block(x); x = Field(x, 1)) {
    count++;
    full_size += sizeof(char*) + caml_string_length(Field(x, 0)) + 1;
  }

  /* Allocate the array of pointers followed by the space to copy the
     strings as one block. */
  result = (char**)malloc(full_size);
  if (result == NULL) caml_raise_out_of_memory();

  ptr = ((char*)result) + (sizeof(char*) * (count + 1));
  for (x = v, i = 0; Is_block(x); x = Field(x, 1), i++) {
    value v_str = Field(x, 0);
    mlsize_t len = caml_string_length(v_str) + 1;
    memcpy(ptr, String_val(v_str), len);
    result[i] = ptr;
    ptr += len;
  }
  result[count] = NULL;

  return result;
}

static char **copy_c_string_array(char ** strings)
{
  char **result;
  size_t count, i, full_size;
  char *ptr;

  count = 0;
  full_size = sizeof(char*);
  while (strings[count] != 0) {
    full_size += sizeof(char*) + strlen(strings[count]) + 1;
    count++;
  }

  /* Allocate the array of pointers followed by the space to copy the
     strings as one block. */
  result = (char**)malloc(full_size);
  if (result == NULL) caml_raise_out_of_memory();

  ptr = (char*)(result + count + 1);
  for (i = 0; i < count; i++) {
    size_t len = strlen(strings[i]) + 1;
    memcpy(ptr, strings[i], len);
    result[i] = ptr;
    ptr += len;
  }
  result[count] = NULL;

  return result;
}

static void free_spawn_info(struct spawn_info *info)
{
  if (info->cwd_kind == PATH) free(info->cwd.path);
  if (info->prog)             free(info->prog);
  if (info->argv)             free(info->argv);
  if (info->env)              free(info->env);
}

extern char ** environ;

enum caml_unix_sigprocmask_command {
  CAML_SIG_SETMASK,
  CAML_SIG_BLOCK,
  CAML_SIG_UNBLOCK,
};

CAMLprim value spawn_unix(value v_env,
                          value v_cwd,
                          value v_prog,
                          value v_argv,
                          value v_stdin,
                          value v_stdout,
                          value v_stderr,
                          value v_use_vfork,
                          value v_setpgid,
                          value v_sigprocmask)
{
  CAMLparam4(v_env, v_cwd, v_prog, v_argv);
  pid_t ret;
  struct spawn_info info;
  int result_pipe[2];
  int cancel_state;
  sigset_t sigset;
  sigset_t saved_procmask;
  struct subprocess_failure failure;
  int got_error = 0;
  int errno_after_forking = 0;
  int status;

  info.std_fds[0] = Int_val(v_stdin);
  info.std_fds[1] = Int_val(v_stdout);
  info.std_fds[2] = Int_val(v_stderr);

  switch (Tag_val(v_cwd)) {
    case 0: /* Path of string */
      assert (Tag_val(Field(v_cwd, 0)) == String_tag);
      info.cwd_kind = PATH;
      info.cwd.path = strdup(String_val(Field(v_cwd, 0)));
      if (info.cwd.path == NULL) caml_raise_out_of_memory();
    break;
    case 1: /* Fd of Unix.file_descr */
      assert (Is_long(Field(v_cwd, 0)));
      info.cwd_kind = FD;
      info.cwd.fd = Int_val(Field(v_cwd, 0));
    break;
    default:
      assert(0);
  }


  info.prog = strdup(String_val(v_prog));
  if (info.prog == NULL) caml_raise_out_of_memory();
  info.argv = alloc_string_vect(v_argv);
  info.env =
    Is_block(v_env) ?
    alloc_string_vect(Field(v_env, 0)) : copy_c_string_array(environ);
  info.set_pgid = Is_block(v_setpgid);
  info.pgid =
    Is_block(v_setpgid) ?
    Long_val(Field(v_setpgid, 0)) : 0;

  caml_enter_blocking_section();
  enter_safe_pipe_section();

  /* Pipe used by the child to send errors to the parent. */
  if (safe_pipe(result_pipe) == -1) {
    int error = errno;
    leave_safe_pipe_section();
    caml_leave_blocking_section();
    free_spawn_info(&info);
    unix_error(error, "pipe", Nothing);
  }

  /* Block signals and thread cancellation. When using vfork, the
     child might share the signal handlers.

     It's not clear that we need the call to [pthread_setcancelstate],
     but implementations of posix_spawn based on vfork are doing this.

     For instance:
     http://git.musl-libc.org/cgit/musl/tree/src/process/posix_spawn.c
  */
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &cancel_state);
  sigfillset(&sigset);
  pthread_sigmask(SIG_SETMASK, &sigset, &saved_procmask);

  if (v_sigprocmask == Val_long(0)) {
    sigemptyset(&info.child_sigmask);
  } else {
    v_sigprocmask = Field(v_sigprocmask, 0);
    value v_sigprocmask_command = Field(v_sigprocmask, 0);
    enum caml_unix_sigprocmask_command sigprocmask_command = Long_val(v_sigprocmask_command);

    switch (sigprocmask_command) {
      case CAML_SIG_SETMASK:
        sigemptyset(&info.child_sigmask);
        break;

      case CAML_SIG_BLOCK:
      case CAML_SIG_UNBLOCK:
        info.child_sigmask = saved_procmask;
        break;

      default:
        caml_failwith("Unknown sigprocmask action");
    }

    for (value v_signals_list = Field(v_sigprocmask, 1);
         v_signals_list != Val_emptylist;
         v_signals_list = Field(v_signals_list, 1)) {
      int signal = caml_convert_signal_number(Long_val(Field(v_signals_list, 0)));
      switch (sigprocmask_command) {
        case CAML_SIG_SETMASK:
        case CAML_SIG_BLOCK:
          sigaddset(&info.child_sigmask, signal);
          break;

        case CAML_SIG_UNBLOCK:
          sigdelset(&info.child_sigmask, signal);
          break;

        default:
          assert(0);
      }
    }
  }

  ret = Bool_val(v_use_vfork) ? vfork() : fork();

  if (ret == 0) {
    close(result_pipe[0]);
    subprocess(result_pipe[1], &info);
  }
  errno_after_forking = errno;

  leave_safe_pipe_section();
  free_spawn_info(&info);
  close(result_pipe[1]);

  got_error = 0;
  if (ret == -1) {
    got_error = 1;
    set_error(&failure, errno_after_forking, "vfork", NOTHING);
  } else {
    intnat res = read(result_pipe[0], &failure, sizeof(failure));
    /* If the sub-process exec successfully, the write end of the
       error pipe is closed (as it has the [O_CLOEXEC] flag set) and
       [read] returns [0].

       If it returns non-zero, it means something went wrong in the
       sub-process and it wrote a [subprocess_failure] structure on
       the pipe. */
    if (res != 0) {
      got_error = 1;
      if (res != sizeof(failure)) {
        /* It's not clear this can happen, but just to be on the safe side */
        set_error(&failure,
                  (res == -1) ? errno : EINVAL,
                  "read",
                  NOTHING);
      };
      /* If [read] did fail for some reason then we might be stuck
         here for a while. Other implementation of posix_spawn just
         assume that [read(...) != sizeof(failure)] is a success. */
      if (got_error) waitpid(ret, &status, 0);
    }
  }

  close(result_pipe[0]);
  pthread_sigmask(SIG_SETMASK, &saved_procmask, NULL);
  pthread_setcancelstate(cancel_state, NULL);

  caml_leave_blocking_section();

  if (got_error) {
    value arg = Nothing;
    switch (failure.arg) {
      case NOTHING: arg = Nothing;         break;
      case CWD    : arg = Field(v_cwd, 0); break;
      case PROG   : arg = v_prog;          break;
    }
    assert(memchr(failure.function, 0, sizeof(failure.function)));
    unix_error(failure.error, failure.function, arg);
  }

  CAMLreturn(Val_int(ret));
}

CAMLprim value spawn_unix_byte(value * argv)
{
  return spawn_unix(argv[0],
                    argv[1],
                    argv[2],
                    argv[3],
                    argv[4],
                    argv[5],
                    argv[6],
                    argv[7],
                    argv[8],
                    argv[9]);
}
