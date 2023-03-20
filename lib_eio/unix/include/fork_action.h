#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* A function that runs in the forked child process. It must not run any OCaml code or invoke the GC.
 * If the action fails then it writes an error message to the FD [errors] and calls [_exit].
 * v_args is the c_action tuple (where field 0 is the function itself).
 */
typedef void fork_fn(int errors, value v_args);

Caml_inline value Val_fork_fn(fork_fn *fn) {
  return caml_copy_nativeint((intnat) fn);
}

/* Run each C action in the list [v_actions].
 * Sets [errors] to be blocking. Never returns.
 */
void eio_unix_run_fork_actions(int errors, value v_actions);

/* Write "$fn: $msg" to fd.
 * fd must be blocking.
 * Ignores failure. */
void eio_unix_fork_error(int fd, char *fn, char *msg);
