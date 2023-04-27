let mono_clock = object
  inherit Eio.Time.Mono.t

  method now = Mtime_clock.now ()

  method sleep_until = Low_level.sleep_until
end

let clock = object
  inherit Eio.Time.clock

  method now = Unix.gettimeofday ()

  method sleep_until time =
    (* todo: use the realtime clock directly instead of converting to monotonic time.
       That is needed to handle adjustments to the system clock correctly. *)
    let d = time -. Unix.gettimeofday () in
    Eio.Time.Mono.sleep mono_clock d
end
