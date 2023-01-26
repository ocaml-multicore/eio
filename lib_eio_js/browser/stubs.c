
#include <stdlib.h>
#include <stdio.h>
void requestIdleCallbackShim () { fprintf(stderr, "Unimplemented Javascript primitive requestIdleCallbackShim!\n"); exit(1); }
void cancelIdleCallbackShim () { fprintf(stderr, "Unimplemented Javascript primitive cancelIdleCallbackShim!\n"); exit(1); }