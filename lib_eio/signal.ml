(*
 * Copyright (C) 2022 Christiano Haesbaert
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

include Config.Signum

type sigbox = {
  box        : unit Stream.t;
  signum     : int;
  hook       : Switch.hook ref;
  subscribed : bool ref;
}

type signum = int

let signum_to_int signum = (signum :> int)

module Private = struct
  type _ Effect.t +=
    | Subscribe : (int * unit Stream.t) -> unit Effect.t
    | Unsubscribe : (int * unit Stream.t) -> unit Effect.t
    | Publish : int -> unit Effect.t

  let subscribe sigbox = Effect.perform @@ Subscribe (sigbox.signum, sigbox.box)
  let unsubscribe sigbox = Effect.perform @@ Unsubscribe (sigbox.signum, sigbox.box)
  let publish signum = Effect.perform @@ Publish signum
end

let check_subscription sigbox =
  if not !(sigbox.subscribed) then
    invalid_arg "sigbox is not subscribed"

let unsubscribe sigbox =
  check_subscription sigbox;
  Private.unsubscribe sigbox;
  sigbox.subscribed := false;
  Switch.remove_hook !(sigbox.hook)

let subscribe ~sw signum =
  assert (signum > 0 && signum < nsig);
  let sigbox =
    { box = Stream.create 1; signum;
      hook = ref Switch.null_hook; subscribed = ref false }
  in
  sigbox.hook := Switch.on_release_cancellable sw (fun () -> unsubscribe sigbox);
  Private.subscribe sigbox;
  sigbox.subscribed := true;
  sigbox

let publish signum = Private.publish signum

let wait sigbox =
  check_subscription sigbox;
  Stream.take sigbox.box

let wait_one signum = Switch.run @@ fun sw -> subscribe ~sw signum |> wait

let is_pending sigbox = not (Stream.is_empty sigbox.box)
