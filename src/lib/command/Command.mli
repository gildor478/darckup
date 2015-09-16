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

(** Command module provides functions to run commands. *)

type filename = string

(** Command line options type. *)
type arg =
  | A of string (* A simple string. *)
  | Fn of string (* A filename, need to be quoted. *)

(** Context for running commands. *)
type 'a t = {
  (** Environment. *)
  env: string array;

  (** Error output. *)
  errf: string -> unit;

  (** Standard output. *)
  outf: string -> unit;

  (** Handle exit code. *)
  exitf: string -> int -> 'a;
}

(** Default context for running commands. This context uses stdout/stderr, the
    default environment and raises a Failure if exit code is non-zero.
  *)
val default: unit t

(** Run a command. *)
val command: 'a t -> string -> 'a

(** Convert exec arguments into a string. *)
val string_of_exec: filename -> arg list -> string

(** Run a command, taking care of escaping filenames, if needed. *)
val exec: 'a t -> filename -> arg list -> 'a
