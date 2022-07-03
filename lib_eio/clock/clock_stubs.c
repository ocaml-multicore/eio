#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <stdint.h>
#include <stdlib.h>

#include "config.h"

#define OCAML_EIO_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("eio.clock: " ERR)); } \
  while (0)

#define NANOS_PER_SECOND 1000000000

/* Detect platform. */
#if defined(__APPLE__) && defined(__MACH__) // MacOS
    #define OCAML_EIO_MACOS
#elif defined(__unix__) || defined(__unix)  // Linux, BSDs
    #define OCAML_EIO_POSIX
    #if defined(__linux__) 
        #define OCAML_EIO_LINUX
    #endif
#elif defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__) // Windows
    #define OCAML_EIO_WINDOWS
#endif

/* system_clock() - system wall clock time
 * mono_clock() : monotonically increasing clock even on system sleep. 
 */
#if defined(HAS_CLOCK_GETTIME_NSEC_NP) // MacOS > 10.12
#include <time.h>

// http://www.manpagez.com/man/3/clock_gettime_nsec_np/
int64_t caml_eio_system_clock_unboxed(value unit) {
    return (int64_t)clock_gettime_nsec_np(CLOCK_REALTIME);
}

int64_t caml_eio_mono_clock_unboxed(value unit) {
    return (int64_t)clock_gettime_nsec_np(CLOCK_MONOTONIC);
}
#elif !defined(HAS_CLOCK_GETTIME_NSEC_NP) && defined(OCAML_EIO_MACOS)
#include <sys/time.h>
#include <mach/mach_time.h>

int64_t caml_eio_system_clock_unboxed(value unit)
{
    struct timeval tv; 

    if(gettimeofday(&tv,(struct timezone *)NULL)) {
        OCAML_EIO_RAISE_SYS_ERROR("gettimeofday() failed");
    }
    return tv.tv_sec * NANOS_PER_SECOND + tv.tv_usec * 1000;
}

// https://developer.apple.com/documentation/kernel/1646199-mach_continuous_time
int64_t caml_eio_mono_clock_unboxed(value unit)
{
    return (int64_t)mach_continuous_time();
}
#elif defined(HAS_CLOCK_GETTIME) // Linux, BSD
#include <time.h>

int64_t caml_eio_gettime(clockid_t id)
{
    struct timespec ts;

    if(clock_gettime(id, &ts)) {
        OCAML_EIO_RAISE_SYS_ERROR("clock_gettime() failed");
    }
    return ts.tv_sec * NANOS_PER_SECOND + ts.tv_nsec;
}

int64_t caml_eio_system_clock_unboxed(value unit)
{
    return caml_eio_gettime(CLOCK_REALTIME);
}

int64_t caml_eio_mono_clock_unboxed(value unit)
{
    clockid_t clock;
#if defined(OCAML_EIO_LINUX)
    clock = CLOCK_BOOTTIME; // https://linux.die.net/man/2/clock_gettime
#elif 
    clock = CLOCK_MONOTONIC; // BSD
#endif
    return caml_eio_gettime(clock);
}
#elif defined(OCAML_EIO_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

int64_t caml_eio_system_clock_unboxed(value unit)
{
    FILETIME time;
    ULARGE_INTEGER large;

    GetSystemTimeAsFile(&system_time);
    large.u.LowPart = time.dwLowDateTime;
    large.u.HighPart = time.dwHighDateTime;
    /* 11,644,473,600,000,000,000: number of nanoseconds between
    the 1st january 1601 and the 1st january 1970 (369 years + 89 leap
    days). */
    return (large.QuadPart * 100 - 11644473600000000000);
}

int64_t caml_eio_mono_clock_unboxed(value unit)
{
    LARGE_INTEGER time;
    static LONGLONG frequecy = 0;
    static double perf_freq;

    if(frequency == 0)
    {
        (void)QueryPerformanceFrequency(&frequency);
        perf_freq = (double)1e9/frequency.QuadPart;
    }
    QueryPerformanceCounter(&time);
    return (int64_t)(time.QuadPart * perf_freq);
}
#else
int64_t caml_eio_system_clock_unboxed(value unit)
{
    return caml_raise_sys_error (caml_copy_string("eio.clock: system_clock() not available"));
}

int64_t caml_eio_mono_clock_unboxed(value unit)
{
    return caml_raise_sys_error (caml_copy_string("eio.clock: mono_clock() not available"));
}
#endif

CAMLprim value caml_eio_system_clock(value unit)
{
    return caml_copy_int64(caml_eio_system_clock_unboxed(unit));
}

CAMLprim value caml_eio_mono_clock(value unit)
{
    return caml_copy_int64(caml_eio_mono_clock_unboxed(unit));
}

double caml_eio_ns_to_seconds_unboxed(value ns)
{
    double d = (double)Int64_val(ns);
    return (d / NANOS_PER_SECOND);
}

CAMLprim value caml_eio_ns_to_seconds(value ns)
{
    return caml_copy_double(caml_eio_ns_to_seconds_unboxed(ns));
}
