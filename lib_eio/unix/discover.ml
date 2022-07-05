module C = Configurator.V1

(* For newer MacOS *)
let clock_gettime_test = {|
#include <time.h>

int main()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return 0;
}
|}

let clock_gettime_nsec_np_test = {|
#include <time.h>

int main()
{
  struct timespec ts;
  clock_gettime_nsec_np(CLOCK_REALTIME);
  return 0;
}
|}

let () = 
  C.main ~name:"clocke_gettime" @@ fun c -> 
  C.C_define.gen_header_file c ~fname:"config.h"
    ["HAS_CLOCK_GETTIME", Switch (C.c_test c clock_gettime_test ~link_flags:["-lrt"]);
     "HAS_CLOCK_GETTIME_NSEC_NP", Switch (C.c_test c clock_gettime_nsec_np_test ~link_flags:["-lrt"]);
    ]
