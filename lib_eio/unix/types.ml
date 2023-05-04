type source = < Eio.Flow.source;  Resource.t; Eio.Flow.close >
type sink   = < Eio.Flow.sink;    Resource.t; Eio.Flow.close >
type socket = < Eio.Flow.two_way; Resource.t; Eio.Flow.close >
