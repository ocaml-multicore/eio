#include <caml/mlvalues.h>

/* A function to convert directory entry kinds into
 * Unix.file_kind options.
 */
CAMLprim value eio_unix_file_type_of_dtype(int d_type);

