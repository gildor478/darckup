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

type filename = string


type arg = A of string | Fn of string

type command_t = ?env:string array ->
  ?errf:(string -> unit) -> ?outf:(string -> unit) -> string -> int


let command ?env ?(errf=prerr_endline) ?(outf=print_endline) cmd =
  let env =
    match env with
    | Some a -> a
    | None -> Unix.environment ()
  in
  let (cmd_out, cmd_in, cmd_err) as proc = Unix.open_process_full cmd env in
  let descr_out = Unix.descr_of_in_channel cmd_out in
  let descr_err = Unix.descr_of_in_channel cmd_err in
  let rec run_until_eof l =
    if l = [] then begin
      match Unix.close_process_full proc with
      | Unix.WEXITED i -> i
      | Unix.WSIGNALED n ->
          failwith
            (Printf.sprintf "Command %S killed by signal %d." cmd n)
      | Unix.WSTOPPED n ->
          failwith
            (Printf.sprintf "Command %S stopped by signal %d." cmd n)
    end else begin
      let ready, _, _ = Unix.select l [] [] 1.0 in
      let l =
        List.fold_left
          (fun l fd ->
             try
               if fd = descr_err then
                 errf (input_line cmd_err)
               else
                 outf (input_line cmd_out);
               l
             with End_of_file ->
               List.filter ((<>) fd) l)
          l ready
      in
        run_until_eof l
    end
  in
    close_out cmd_in;
    run_until_eof [descr_out; descr_err]


let exec ?env ?errf ?outf bin args =
  let l =
    List.map
      (function
         | A s -> s
         | Fn fn -> Filename.quote fn)
      (Fn bin :: args)
  in
    command ?env ?errf ?outf (String.concat " " l)
