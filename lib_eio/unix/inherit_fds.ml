(*
 * Copyright (C) 2023 Thomas Leonard
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

module M = Map.Make(Int)

module Count = struct
  let create () = ref M.empty

  let get t fd =
    M.find_opt fd !t
    |> Option.value ~default:0

  let incr t fd =
    let inc x = Some (1 + Option.value x ~default:0) in
    t := M.update fd inc !t

  let decr t fd =
    match get t fd with
    | i when i <= 0 -> assert false
    | 1 -> t := M.remove fd !t; `Unused
    | i -> t := M.add fd (pred i) !t; `Still_needed
end

type action = { src : int; dst : int }

let plan mapping =
  let mapping =
    List.fold_left (fun acc (dst, src) ->
        if M.mem dst acc then Fmt.invalid_arg "FD %d assigned twice!" dst;
        M.add dst src acc
      ) M.empty mapping
  in
  let plan = ref [] in
  let dup2 src dst = plan := {src; dst} :: !plan in
  let users_of = Count.create () in
  (* First, for any FDs that map to themselves we emit (fd, fd) and then forget about it,
     as this doesn't interfere with anything else.
     We also set [users_of] to track how many times each FD is needed. *)
  let mapping = mapping |> M.filter (fun dst src ->
      if src = dst then (dup2 src src; false)        (* Just clear the close-on-exec flag. *)
      else (Count.incr users_of src; true)
    ) in
  let tmp = ref (-1) in (* The FD we dup'd to the temporary FD when breaking cycles. *)
  let rec no_users dst =
    (* Nothing requires the old value of [dst] now,
       so if we wanted to put something there, do it. *)
    M.find_opt dst mapping |> Option.iter (fun src -> dup src dst)
  and dup src dst =
    (* Duplicate [src] as [dst]. *)
    if src = !tmp then (
      (* We moved [src] to [tmp] to break a cycle, so use [tmp] instead.
         Also, there's nothing to do after this as the cycle is broken. *)
      dup2 (-1) dst;
    ) else (
      dup2 src dst;
      (* Record that [dst] no longer depends on [src]. *)
      match Count.decr users_of src with
      | `Still_needed -> ()
      | `Unused -> no_users src
    )
  in
  (* Find any loose ends and work backwards.
     Note: we need to do this in two steps because [dup] modifies [users_of]. *)
  mapping
  |> M.filter (fun dst _src -> Count.get users_of dst = 0)   (* FDs with no dependants *)
  |> M.iter (fun dst src -> dup src dst);
  (* At this point there are no loose ends; we have nothing but cycles left. *)
  (* M.iter (fun _ v -> assert (v = 1)) !users_of; *)
  (* For each cycle, break it at one point using the temporary FD.
     It's safe to allocate the temporary FD now because every FD we plan to use is already allocated. *)
  let rec break_cycles () =
    match M.min_binding_opt !users_of with   (* Pick any remaining FD. *)
    | None -> ()
    | Some (src, _) ->
      dup2 src (-1);            (* Duplicate [src] somewhere. *)
      tmp := src;               (* Remember that when we try to use it later. *)
      (* The FD that needed [src] can now use [tmp] instead: *)
      let state = Count.decr users_of src in
      assert (state = `Unused);
      no_users src;             (* Free this cycle. *)
      break_cycles ()           (* Free any other cycles. *)
  in
  break_cycles ();
  List.rev !plan
