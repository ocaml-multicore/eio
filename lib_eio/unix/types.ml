type source_ty = [`Unix_fd | Eio.Resource.close_ty | Eio.Flow.source_ty]
type sink_ty   = [`Unix_fd | Eio.Resource.close_ty | Eio.Flow.sink_ty]
type 'a source = ([> source_ty] as 'a) Eio.Resource.t
type 'a sink = ([> sink_ty] as 'a) Eio.Resource.t
