
(** Command module provides functions to run commands. *)

type filename = string

(** Command line options type. *)
type arg = 
  | A of string (* A simple string. *)
  | Fn of string (* A filename, need to be quoted. *)

type command_t = ?env:string array ->
  ?errf:(string -> unit) -> ?outf:(string -> unit) -> string -> int

(** Run a command. *)
val command : command_t

(** Run a command, taking care of escaping filenames, if needed. *)
val exec :
  ?env:string array ->
  ?errf:(string -> unit) ->
  ?outf:(string -> unit) -> filename -> arg list -> int
