type t = {
  sysname : string;
  release : string;
  version : string;
  machine : string;
}

external eio_uname : unit -> t = "eio_unix_uname"

let v = eio_uname ()

let dump =
  Fmt.Dump.(record [
      field "sysname" (fun t -> t.sysname) string;
      field "release" (fun t -> t.release) string;
      field "version" (fun t -> t.version) string;
      field "machine" (fun t -> t.machine) string;
    ])
