(** Provides information about the system from e.g. uname(2). *)

type t = {
  sysname : string;
  release : string;
  version : string;
  machine : string;
}
(** Examples:

    {[
    { "sysname" = "Linux";        
      "release" = "7.2.8";
      "version" = "#1-NixOS SMP PREEMPT_DYNAMIC Fri Sep 25 14:37:14 UTC 2026";
      "machine" = "x86_64" }

    { "sysname" = "FreeBSD";
      "release" = "15.1-RELEASE";
      "version" = "FreeBSD 15.1-RELEASE releng/15.1-n283562-96841ea08dcf GENERIC";
      "machine" = "amd64" }

    { "sysname" = "Darwin";
      "release" = "24.5.0";
      "version" =
      "Darwin Kernel Version 24.5.0: Tue Apr 22 19:48:46 PDT 2025; root:xnu-11417.121.6~2/RELEASE_ARM64_T8103";
      "machine" = "arm64" }

    { "sysname" = "OpenBSD";
      "release" = "7.9";
      "version" = "GENERIC.MP#449";
      "machine" = "amd64" }

    { "sysname" = "Windows";
      "release" = "";
      "version" = "";
      "machine" = "" }
    ]}
*)

val v : t
(** Information about the host system. *)

val dump : t Fmt.t
