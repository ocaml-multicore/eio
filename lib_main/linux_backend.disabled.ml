let run ~config:_ ~fallback _ = fallback (`Msg "The io_uring backend was disabled at compile-time")
