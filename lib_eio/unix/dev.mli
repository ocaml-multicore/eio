(** Unix device numbers.

    This is the Unix view of {!Eio.File.Dev.t}, to split the device into a
    major number identifying the driver and the minor number identifying the
    device it controls. How many bits each gets is platform-specific, so a
    device number made on one system should not be used on another.

    @since 1.5 *)

type t = Eio.File.Dev.t

val make : major:int -> minor:int -> t
(** [make ~major ~minor] combines the two numbers (see [makedev(3)]).

    @raise Invalid_argument if either number is negative, or does not fit in
           the bits this platform gives it. *)

val major : t -> int
(** [major t] is the major number of [t]. *)

val minor : t -> int
(** [minor t] is the minor number of [t]. *)

val pp : t Fmt.t
(** [pp] formats a device number as [major:minor]. *)
