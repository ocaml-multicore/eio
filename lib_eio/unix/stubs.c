#include "primitives.h"

#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <errno.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#define CAML_INTERNALS
#include <caml/runtime_events.h> // for CAML_EV_BEGIN and CAML_EV_END
#include <caml/sys.h> // for caml_strerror
#undef CAML_INTERNALS

CAMLprim value eio_unix_is_blocking(value v_fd) {
  #ifdef _WIN32
  // We should not call this function from Windows
  uerror("Unsupported blocking check on Windows", Nothing);
  #else
  int fd = Int_val(v_fd);
  int r = fcntl(fd, F_GETFL, 0);
  if (r == -1)
    uerror("fcntl", Nothing);

  return Val_bool((r & O_NONBLOCK) == 0);
  #endif
}

typedef pthread_cond_t* sync_condvar;
#define Condition_val(v) (* (sync_condvar *) Data_custom_val(v))
typedef pthread_mutex_t * sync_mutex;
#define Mutex_val(v) (* ((sync_mutex *) Data_custom_val(v)))

static void sync_check_error(int retcode, char* msg)
{
  char* err;
  char buf[1024];
  int errlen, msglen;
  value str;

  if (retcode == 0) return;
  if (retcode == ENOMEM) caml_raise_out_of_memory();
  err = caml_strerror(retcode, buf, sizeof(buf));
  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memcpy (&Byte(str, 0), msg, msglen);
  memcpy (&Byte(str, msglen), ": ", 2);
  memcpy (&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}

CAMLprim value eio_unix_condition_timedwait(value wcond, value wmut, value vuntil)
{
  CAMLparam3(wcond, wmut, vuntil);
  sync_condvar cond = Condition_val(wcond);
  sync_mutex mut = Mutex_val(wmut);
  double until = Double_val(vuntil);
  struct timespec ts;

  ts.tv_sec = until;
  ts.tv_nsec = (until - ts.tv_sec) * 1e9;

  CAML_EV_BEGIN(EV_DOMAIN_CONDITION_WAIT);
  caml_enter_blocking_section();
  int retcode = pthread_cond_timedwait(cond, mut, &ts);
  caml_leave_blocking_section();
  CAML_EV_END(EV_DOMAIN_CONDITION_WAIT);
  if (retcode == ETIMEDOUT) {
    CAMLreturn(Val_false);
  }
  sync_check_error(retcode, "Condition.timed_wait");

  CAMLreturn(Val_true);
}
