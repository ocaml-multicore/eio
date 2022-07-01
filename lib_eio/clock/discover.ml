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

let () = 
  C.main ~name:"clocke_gettime" @@ fun c -> 
  C.C_define.gen_header_file c ~fname:"config.h"
    ["HAS_CLOCK_GETTIME", Switch (C.c_test c clock_gettime_test ~link_flags:["-lrt"]);
    ]
