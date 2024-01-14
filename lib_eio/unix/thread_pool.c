/* This file is mostly taken from OCaml PR#12867.
 * It exposes pthread_cond_timedwait(3) for timed_semaphore.ml.
 * This file (and timed_semaphore.ml) can both be deleted once
 * this feature is available in OCaml itself.
*/

#include <pthread.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define CAML_INTERNALS
#include <caml/runtime_events.h> // for CAML_EV_BEGIN and CAML_EV_END
#include <caml/sys.h> // for caml_strerror
#if defined(_WIN32)
#include <caml/winsupport.h> // for CAML_ULONGLONG_FILETIME
#endif
#undef CAML_INTERNALS


#if defined(_WIN32)
/* There are 11644473600 seconds between 1 January 1601 (the NT Epoch) and 1
 * January 1970 (the Unix Epoch). FILETIME is measured in 100ns ticks.
 */
#define CAML_NT_EPOCH_100ns_TICKS 116444736000000000ULL
#else
#include <sys/time.h>
#endif

typedef pthread_cond_t* sync_condvar;
#define Condition_val(v) (* (sync_condvar *) Data_custom_val(v))
typedef pthread_mutex_t * sync_mutex;
#define Mutex_val(v) (* ((sync_mutex *) Data_custom_val(v)))

static void sync_check_error(int retcode, char * msg)
{
  char * err;
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

#if defined(_WIN32)
static inline void sync_populate_timespec(double timeout_sec, struct timespec * ts)
{
  double integr, frac, until;
  CAML_ULONGLONG_FILETIME utime;

  GetSystemTimeAsFileTime(&utime.ft);
  until = (utime.ul - CAML_NT_EPOCH_100ns_TICKS) * 1e-7 + timeout_sec;
  frac = modf(until, &integr);
  ts->tv_sec = (time_t)integr;
  ts->tv_nsec = ceil(1e9 * frac);
}
#else
static inline void sync_populate_timespec(double timeout_sec, struct timespec * ts)
{
  double integr, frac;
  frac = modf(timeout_sec, &integr);

  struct timeval tv;
  gettimeofday(&tv, 0);
  ts->tv_sec = tv.tv_sec + (time_t)integr;
  ts->tv_nsec =
    (uint64_t)tv.tv_usec * (uint64_t)1000 +
    (uint64_t)ceil(1e9 * frac);

  if (ts->tv_nsec >= 1e9) {
    ts->tv_sec++;
    ts->tv_nsec -= 1e9;
  }
}
#endif

value eio_unix_condition_timedwait(value v_cond, value v_mut,
                                           value v_timeout_sec)
{
  CAMLparam3(v_cond, v_mut, v_timeout_sec);
  sync_condvar cond = Condition_val(v_cond);
  sync_mutex mut = Mutex_val(v_mut);
  double timeout_sec = Double_val(v_timeout_sec);
  int retcode;
  struct timespec ts;

  sync_populate_timespec(timeout_sec, &ts);

  CAML_EV_BEGIN(EV_DOMAIN_CONDITION_WAIT);
  caml_enter_blocking_section();
  retcode = pthread_cond_timedwait(cond, mut, &ts);
  caml_leave_blocking_section();
  if (retcode == ETIMEDOUT) {
    CAMLreturn(Val_false);
  }
  sync_check_error(retcode, "Condition.timed_wait");
  CAML_EV_END(EV_DOMAIN_CONDITION_WAIT);

  CAMLreturn(Val_true);
}
