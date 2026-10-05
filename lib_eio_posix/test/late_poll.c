#define _GNU_SOURCE
#include <dlfcn.h>
#include <poll.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

static void late_sigchld(int blocking) {
  static int sent;
  if (blocking && !sent) {
    sent = 1;
    kill(getpid(), SIGCHLD);
  }
}

int poll(struct pollfd *fds, nfds_t nfds, int timeout) {
  static int (*real)(struct pollfd *, nfds_t, int);
  if (!real) real = dlsym(RTLD_NEXT, "poll");
  late_sigchld(timeout != 0);
  return real(fds, nfds, timeout);
}

int ppoll(struct pollfd *fds, nfds_t nfds, const struct timespec *timeout, const sigset_t *sigmask) {
  static int (*real)(struct pollfd *, nfds_t, const struct timespec *, const sigset_t *);
  if (!real) real = dlsym(RTLD_NEXT, "ppoll");
  late_sigchld(!timeout || timeout->tv_sec || timeout->tv_nsec);
  return real(fds, nfds, timeout, sigmask);
}
