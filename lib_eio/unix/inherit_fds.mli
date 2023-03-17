(** Plan how to renumber FDs in a child process. *)

type action = { src : int; dst : int }
(** { src; dst} is (roughly) a request to [dup2(src, dst)].

    [dst] should not be marked as close-on-exec.
    If [src = dst] then simply clear the close-on-exec flag for the FD.

    An FD of -1 means to use a temporary FD (e.g. use [dup] the first time,
    with close-on-exec true). This is needed if there are cycles (e.g. we want
    to switch FDs 1 and 2). Only one temporary FD is needed at a time, so it
    can be reused as necessary. *)

val plan : (int * int) list -> action list
(** [plan mapping] calculates a sequence of operations to renumber file descriptors so that
    FD x afterwards refers to the object that [List.assoc x mapping] referred to at the start.

    It returns a list of actions to be performed in sequence.
    Example: [plan [1, 2]] is just [[(2, 1)]]. *)
