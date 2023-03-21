let src = Logs.Src.create "eio_linux" ~doc:"Effect-based IO system for Linux/io-uring"
include (val Logs.src_log src : Logs.LOG)
