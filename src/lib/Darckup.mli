

(** Darckup module provides function to manage a dar backup system. *)

(** Type describing a filename. *)
type filename = string


(** Archive module provides types and functions to manipulate full backup
    archives and incremental backup archives. It only acts on naming convention
    and doesn't actually read the content of the file,
  *)
module Archive:
sig
  type t

  val is_full: t -> bool

  val to_filenames: t -> filename list
end

module ArchiveSet:
sig
  type t

  (** [of_filenames] returns the archive representation of the filenames and
      the list of rejected filenames.
    *)
  val of_filenames: filename list -> t * filename list

  val to_filenames: t -> filename list

  (** [lenght] returns the number of archive. *)
  val length: t -> int

  val last: t -> Archive.t

  val next: t -> int -> filename -> int -> Archive.t

  val pop: t -> t * Archive.t

  val push: t -> Archive.t -> t

  (** [list] returns the list of archive given a directory name. *)
  val list: filename -> t * filename list
end
