#require "eio_main";;

module Eio_main = struct
  open Eio.Std

  let run fn =
    Eio_main.run @@ fun env ->
    try fn env
    with Failure msg -> traceln "Error: %s" msg
end
