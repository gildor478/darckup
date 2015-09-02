

(** Darckup module provides function to manage a dar backup system. *)

(** Type describing a filename. *)
type filename = string

(** Archive module provides types and functions to manipulate full and
  * incremental backup archive filename. It only acts on naming convention
    and doesn't actually read the content of the file,
  *)
module Archive:
sig
  type t

  val is_full: t -> bool

  val to_filenames: t -> filename list
end

(** ArchiveSet module provides types and functions to manipulate a set of full
    and incremental backup archive filenames. It only acts on naming convention
    and doesn't actually read the content of the file,
  *)
module ArchiveSet:
sig
  type t

  (** [of_filenames lst] returns the archive representation of the filenames and
      the list of rejected filenames.
    *)
  val of_filenames: filename list -> t * filename list

  (** [to filenames t] returns the sorted list of filename in this archive set.
    *)
  val to_filenames: t -> filename list

  (** [lenght] returns the number of archive.
    *)
  val length: t -> int

  (** [last t] returns the last existing archive.
    *)
  val last: t -> Archive.t

  (** [next t max_incremental prefix_full] returns either the latest prefix
      with an incremented suffix or [prefix_full ^ "full"].
    *)
  val next: t -> int -> filename -> filename

  (** [pop t] returns the archive set without the first element, which is
      returned as well.
    *)
  val pop: t -> t * Archive.t

  (** [npop n t] returns the first [n] archives from the set and the updated
      archive set.
    *)
  val npop: int -> t -> t * Archive.t list
end

type archive_set = {
  backup_dir: filename;
  darrc: filename;
  prefix: string;
  max_incrementals: int;
  max_archives: int;
}

type t = {
  dar: filename;
  now_rfc3339: string;
  pre_command: string option;
  post_command: string option;
  ignore_files: string list;
  archive_sets: (string * archive_set) list;

  (* System interface. *)
  command: string -> int;
  readdir: filename -> filename array;
  remove: filename -> unit;
  getcwd: unit -> filename;
  file_exists: filename -> bool;
  is_directory: filename -> bool;
  log: [`Info | `Warning | `Error] -> string -> unit;
}

val default: t

val load: t -> filename -> t

val create: t -> unit

val clean: t -> unit
