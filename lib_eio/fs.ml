(** Note: file-system operations, such as opening or deleting files,
    can be found in the {!Path} module. *)

open Std

(** {2 Types} *)

type path = string

type error =
  | Already_exists of Exn.Backend.t
  | Not_found of Exn.Backend.t
  | Permission_denied of Exn.Backend.t
  | File_too_large
  | Not_native of string          (** Raised by {!Path.native_exn}. *)

type Exn.err += E of error

let err e =
  Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
      | E e ->
        Fmt.string f "Fs ";
        begin match e with
          | Already_exists e -> Fmt.pf f "Already_exists %a" Exn.Backend.pp e
          | Not_found e -> Fmt.pf f "Not_found %a" Exn.Backend.pp e
          | Permission_denied e -> Fmt.pf f "Permission_denied %a" Exn.Backend.pp e
          | File_too_large -> Fmt.pf f "File_too_large"
          | Not_native m -> Fmt.pf f "Not_native %S" m
        end;
        true
      | _ -> false
    )

(** When to create a new file. *)
type create = [
  | `Never                                 (** fail if the named file doesn't exist *)
  | `If_missing of File.Unix_perm.t        (** create if file doesn't already exist *)
  | `Or_truncate of File.Unix_perm.t       (** any existing file is truncated to zero length *)
  | `Exclusive of File.Unix_perm.t         (** always create; fail if the file already exists *)
]
(** If a new file is created, the given permissions are used for it. *)

type dir_ty = [`Dir]
type 'a dir = ([> dir_ty] as 'a) r
(** Note: use the functions in {!Path} to access directories. *)

(** {2 Provider Interface} *)

module Pi = struct
  module type DIR = sig
    type t

    val open_in : t -> sw:Switch.t -> path -> File.ro_ty r

    val open_out :
      t ->
      sw:Switch.t ->
      append:bool ->
      create:create ->
      path -> File.rw_ty r

    val mkdir : t -> perm:File.Unix_perm.t -> path -> unit
    val open_dir : t -> sw:Switch.t -> path -> [`Close | dir_ty] r
    val read_dir : t -> path -> string list
    val stat : t -> follow:bool -> string -> File.Stat.t
    val unlink : t -> path -> unit
    val rmdir : t -> path -> unit
    val rename : t -> path -> _ dir -> path -> unit
    val read_link : t -> path -> string
    val symlink : t -> path -> path -> unit
    val pp : t Fmt.t
    val native : t -> string -> string option
  end

  type (_, _, _) Resource.pi +=
    | Dir : ('t, (module DIR with type t = 't), [> dir_ty]) Resource.pi
end
