#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <stdint.h>
#include <stdlib.h>

#include "config.h"

#define OCAML_EIO_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("eio.clock: " ERR)); } \
  while (0)

/* Detect platform. */
#if defined(__APPLE__) && defined(__MACH__) // MacOS
    #define OCAML_EIO_MACOS
#elif defined(__unix__) || defined(__unix)  // Linux, BSDs
    #define OCAML_EIO_POSIX
#elif defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__) // Windows
    #define OCAML_EIO_WINDOWS
#endif

/* Determine system clock implementation. */
#if !defined(HAS_CLOCK_GETTTIME)
    #define SYSTEM_GETTIMEOFDAY
#elif defined(HAS_CLOCK_GETTIME)
    #define SYSTEM_CLOCK_GETTIME
#elif defined(OCAML_EIO_WINDOWS)
    #define SYSTEM_WINDOWS
#endif 

/* system clock : gettimeofday() */
#if defined(SYSTEM_GETTIMEOFDAY)
#include <sys/time.h>

int64_t caml_eio_system_clock_unboxed(value unit)
{
    struct timeval tv; 

    if(gettimeofday(&tv,(struct timezone *)NULL)) {
        OCAML_EIO_RAISE_SYS_ERROR("gettimeofday() failed.");
    }
    return tv.tv_sec * 1000000000 + tv.tv_usec * 1000;
}
#endif

/* system clock: clock_gettime() */
#if defined(SYSTEM_CLOCK_GETTIME)
#include <time.h>

int64_t caml_eio_system_clock_unboxed(value unit)
{
    struct timespec ts;

    if(clock_gettime(CLOCK_REALTIME,&ts)) {
        OCAML_EIO_RAISE_SYS_ERROR("clock_gettime(CLOCK_REALTIME) failed.");
    }
    return ts.tv_sec * 1000000000 + ts.tv_nsec;
}
#endif

/* system clock: GetSystemTimeAsFileTime() */
#if defined(SYSTEM_WINDOWS)
#include <windows.h>

int64_t caml_eio_system_clock_unboxed(value unit)
{
    FILETIME system_time;
    ULARGE_INTEGER large;

    GetSystemTimeAsFile(&system_time);
    large.u.LowPart = system_time.dwLowDateTime;
    large.u.HighPart = system_time.dwHighDateTime;
    /* 11,644,473,600,000,000,000: number of nanoseconds between
    the 1st january 1601 and the 1st january 1970 (369 years + 89 leap
    days). */
     return (large.QuadPart * 100 - 11644473600000000000);
}
#endif

CAMLprim value caml_eio_system_clock(value unit)
{
    return caml_copy_int64(caml_eio_system_clock_unboxed(unit));
}
