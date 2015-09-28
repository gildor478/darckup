(*************************************************************************)
(* darckup: Manage dar backups.                                          *)
(*                                                                       *)
(* Copyright (C) 2015, Sylvain Le Gall                                   *)
(*                                                                       *)
(* This program is free software: you can redistribute it and/or modify  *)
(* it under the terms of the GNU General Public License as published by  *)
(* the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                   *)
(*                                                                       *)
(* This program is distributed in the hope that it will be useful,       *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(* GNU General Public License for more details.                          *)
(*                                                                       *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*************************************************************************)


(** Darckup module provides functions to manage a dar backup system. *)

(** Type describing a filename. *)
type filename = string

(** Archive module provides types and functions to manipulate full and
  * incremental backup archive filename. It only acts on naming convention
    and doesn't actually read the content of the file,
  *)
module Archive:
sig
  type t

  (** Test if this is a full archive.
    *)
  val is_full: t -> bool

  (** Return a prefix suitable to use with -A option of dar, for incremental
      archive.
    *)
  val to_full_prefix: t -> filename

  (** Return a prefix suitable to use with -c option of dar.
    *)
  val to_prefix: t -> filename

  (** Return the list of filenames for this archive.
    *)
  val to_filenames: t -> filename list

  (** Parse a string to build an archive.
    *)
  val parse: string -> t

  (** Merge two archive, if their prefix match.
    *)
  val merge: t -> t -> t
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

  (** [next t max_incremental short_prefix] returns the archive that should be
      created after the last one.
    *)
  val next: t -> int -> filename -> Archive.t

  (** [pop t] returns the archive set without the first element, which is
      returned as well.
    *)
  val pop: t -> t * Archive.t

  (** [npop n t] returns the first [n] archives from the set and the updated
      archive set.
    *)
  val npop: int -> t -> t * Archive.t list
end

(** Definition of hooks to run for clean/create commands.
  *)
type hooks = {
  (** Command to run before create.
    *)
  pre_create_command: string option;

  (** Command to run after create.
    *)
  post_create_command: string option;

  (** Command to run before clean.
    *)
  pre_clean_command: string option;

  (** Command to run after clean.
    *)
  post_clean_command: string option;
}

(** Definition of an archive_set, as defined in an INI file.
  *)
type archive_set = {
  (** Directory wher the archives are stored.
    *)
  backup_dir: filename;

  (** Configuration file for dar, for these archives.
    *)
  darrc: filename;

  (** A prefix with which the archive name will start.
    *)
  base_prefix: string;

  (** Maximal number of incremental archives (create).
    *)
  max_incrementals: int;

  (** Maximal number of archives (clean).
    *)
  max_archives: int;

  (** Hooks to run when handling this archive_set.
    *)
  archive_set_hooks: hooks;
}

(** Configuration for Darckup high level commands. *)
type t = {
  (** The dar binary.
    *)
  dar: filename;

  (** A time string that will be used to compose name of new full archive.
    *)
  now_rfc3339: string;

  (** Don't run, just simulate running.
    *)
  dry_run: bool;

  (** File to ignore when scanning backup directories when scanning
      for archives.
    *)
  ignore_glob_files: string list;

  (** Global hooks to run.
    *)
  global_hooks: hooks;

  (** All the available archives and the name of each.
    *)
  archive_sets: (string * archive_set) list;

  (** System interface.
    *)
  command: unit Command.t -> string -> unit;
  exec: unit Command.t -> filename -> Command.arg list -> unit;
  readdir: filename -> filename array;
  remove: filename -> unit;
  getcwd: unit -> filename;
  file_exists: filename -> bool;
  is_directory: filename -> bool;
  log: [`Debug | `Info | `Warning | `Error] -> string -> unit;
}

(** Default configuration. *)
val default: t

(** [load_configuration t ~dir ini] Merge configuration found in [ini] and then
    all '.ini' files in [~dir], into the configuration [t].
  *)
val load_configuration: t -> ?dir:filename -> filename -> t

(** [load_archive_sets t] Transform [t.archive_sets] into [ArchiveSet.t list].
  *)
val load_archive_sets: t -> (string * archive_set * ArchiveSet.t) list

(** Create archives and return the created archives.
  *)
val create: t -> (string * Archive.t) list

(** Clean up archives, to match max archives constraint.
  *)
val clean: t -> unit

(** An environment variable.
  *)
type variable = string

(** Retrieve value from environment variable.
  *)
val getenv: t -> ?current_archive_set:string -> variable -> string

(** List of available variables and a short help text.
  *)
val getenv_variables: (variable * string) list
