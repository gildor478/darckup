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

open Darckup
open Cmdliner


type copts =
  {
    logging_filter: [`Error|`Warning|`Info|`Debug] -> bool;
    cdry_run: bool;
    cdar: string option;
    cnow_rfc3339: string option;
    cno_terminal: bool;
    ini: filename;
    ini_d: filename;
  }


let variables_secs =
  [`S "VARIABLES";
   `P "List of available variables:"]
  @
  (List.fold_left
     (fun lst (nm, help) ->
        `I ("$(b,"^nm^")", help) ::
        (if lst <> [] then `Noblank :: lst else []))
     [] Darckup.hook_variables)


let copts_sect = "COMMON OPTIONS"
let help_secs =
  [`S copts_sect;
   `P "These options are common to all commands.";
   `S "MORE HELP";
   `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
   `Noblank;
   `P "Use `$(mname) help hook' for help on hook execution.";
   `S "BUGS";
   `P "Check bug reports at https://forge.ocamlcore.org/projects/darckup/."]


let help copts man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None)
  | Some topic ->
      let topics = "topics" :: "hook" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
        | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
        | `Ok t ->
            let page =
              (String.uppercase ("darckup-"^topic), 7, "",
               "Darckup "^Conf.version,
               "Darckup Manual"),
              [`S "HOOK EXECUTION";
               `P "Darckup will automatically execute somes commands, before and
                   after $(b,create)/$(b,clean) commands. There will be one
                   global execution and one execution per archive_set.";
               `P "Command are defined through the following variables:";
               `I ("$(b,default.pre_create_command)",
                   "Command to run before $(b,create)."); `Noblank;
               `I ("$(b,default.post_create_command)",
                   "Command to run after $(b,create)."); `Noblank;
               `I ("$(b,default.pre_clean_command)",
                   "Command to run before $(b,clean)."); `Noblank;
               `I ("$(b,default.post_clean_command)",
                   "Command to run after $(b,clean)."); `Noblank;
               `I ("$(b,archive_set.pre_create_command)",
                   "Command to run before $(b,create) for the given
                    archive_set."); `Noblank;
               `I ("$(b,archive_set.post_create_command)",
                   "Command to run after $(b,create) for the given
                    archive_set."); `Noblank;
               `I ("$(b,archive_set.pre_clean_command)",
                   "Command to run before $(b,clean) for the given
                    archive_set. Run for every archive to remove."); `Noblank;
               `I ("$(b,archive_set.post_clean_command)",
                   "Command to run after $(b,clean) for the given
                    archive_set. Run for every archive to remove.");

               `S "DATA RETRIEVAL";
               `P "It is possible to extract data about the current context of
                   execution for the command to be executed. The first method to
                   extract date is to use substition of the command line, the
                   second method is to call back '$DARCKUP getenv
                   current.backup_dir' from the script executed.";

               `P "Command line substitution:"; `Noblank;
               `P "  post_create_command=touch 
                      $(\\$){current.last.prefix}.done";

               `P "From a script:"; `Noblank;
               `P "  post_create_command=script.sh";

               `P "And $(i,script.sh)"; `Noblank;
               `P "  #!/bin/sh"; `Noblank;
               `P "  touch `$(\\$)DARCKUP getenv current.last.prefix`.done";
              ] @ variables_secs

            in
              `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)


let t ?(asopts=fun _ -> true) copts =
  let open Darckup in
  let t =
    load_configuration default ~dir:copts.ini_d copts.ini
  in
  let if_opt e = function Some e -> e | None -> e in
    {
      t with
          archive_sets = List.filter (fun  (s, _) -> asopts s) t.archive_sets;
          dar = if_opt t.dar copts.cdar;
          dry_run = copts.cdry_run;
          no_terminal = copts.cno_terminal;
          now_rfc3339 = if_opt t.now_rfc3339 copts.cnow_rfc3339;
          log = (fun lvl s ->
                   if copts.logging_filter lvl then
                     Printf.eprintf "%c: %s\n%!"
                       (List.assoc lvl
                          [`Debug, 'D';
                           `Info, 'I';
                           `Warning, 'W';
                           `Error, 'E'])
                       s);
    }


let list copts =
  List.iter
    (fun (s, _) -> print_endline s)
    (t copts).Darckup.archive_sets;
  `Ok ()


let list_archive copts aset_lst =
  let module S = Set.Make(String) in
  let set_of_list =  List.fold_left (fun set a -> S.add a set) S.empty in
  let set = set_of_list aset_lst in
  let t = t ~asopts:(fun s -> S.mem s set) copts in
  let set_ref = set_of_list (List.rev_map fst t.archive_sets) in
  let diff = S.diff set set_ref in
    if diff <> S.empty then begin
      failwith
        (Printf.sprintf
           "Non-existing archive_set(s): %s"
           (String.concat ", " (S.elements diff)))
    end;
    List.iter
      (fun (_, _, a) ->
         List.iter print_endline (ArchiveSet.to_filenames a))
      (Darckup.load_archive_sets t);
    `Ok ()


let create copts asopts =
  let _lst: (string * Archive.t) list =
    Darckup.create (t ~asopts copts)
  in
    `Ok ()


let clean copts asopts =
  let () =
    Darckup.clean (t ~asopts copts)
  in
    `Ok ()


let cronjob copts asopts =
  let copts = {copts with cno_terminal = true} in
  match create copts asopts with
  | `Ok () -> clean copts asopts
  |  res -> res


(*
 * Options common to all commands
 *)


let copts_t =
  let docs = copts_sect in
  (** Logging definition. *)
  let normal =
    function
      | `Warning | `Error -> true
      | `Debug | `Info -> false
  in
  let quiet =
    let doc = "No logging." in
      (fun _ -> false),
      Arg.info ["quiet"] ~docs ~doc
  in
  let verbose =
    let doc = "Verbose logging (up to info level)." in
      ((<>) `Debug),
      Arg.info ["verbose"] ~docs ~doc
  in
  let debug =
    let doc = "Debug logging (up to debug level)." in
      (fun _ -> true),
      Arg.info ["debug"] ~docs ~doc
  in
  let logging_filter =
    Arg.(last & vflag_all [normal] [quiet; verbose; debug])
  in
  (** Other common options. *)
  let dry_run =
    let doc = "Just display what should be done." in
      Arg.(value & flag & info ["dry_run"] ~docs ~doc)
  in
  let dar =
    let doc = "The $(i,dar) executable." in
      Arg.(value & opt (some file) None & info ["dar"] ~docs ~doc)
  in
  let now_rfc3339 =
    let doc = "The date prefix used to generate new archive." in
      Arg.(value & opt (some string) None & info ["now_rfc3339"] ~docs ~doc)
  in
  let ini =
    let doc = "INI file to load for configuration." in
      Arg.(value & opt file Conf.ini & info ["ini"] ~docs ~doc)
  in
  let ini_d =
    let doc = "Directory containing INI files to load for configuration." in
      Arg.(value & opt dir Conf.ini_d & info ["ini_d"] ~docs ~doc)
  in
  let no_terminal =
    let doc = "No terminal available for user interaction." in
      Arg.(value & flag & info ["no_terminal"] ~docs ~doc)
  in
  let copts logging_filter dry_run dar now_rfc3339 ini ini_d no_terminal =
    {
      logging_filter;
      cdry_run = dry_run;
      cdar = dar;
      cnow_rfc3339 = now_rfc3339;
      cno_terminal = no_terminal;
      ini;
      ini_d
    }
  in
    Term.(pure copts $ logging_filter $ dry_run $ dar
            $ now_rfc3339 $ ini $ ini_d $ no_terminal)


(*
 * Options to select an archive_set.
 *)


let asopts_sect = "ARCHIVE_SET SELECTION OPTIONS"
let asopts_t =
  let docs = asopts_sect in
  let all_archive_sets =
    let doc = "Select all archive_sets." in
      Arg.(value & flag & info ["all_archive_sets"] ~docs ~doc)
  in
  let archive_set =
    let doc = "Select one archive_set." in
      Arg.(value & (opt_all string []) & info ["archive_set"] ~docs ~doc)
  in
  let copts all_archive_sets archive_sets =
    if all_archive_sets then begin
      fun _ -> true
    end else begin
      fun s -> List.mem s archive_sets
    end
  in
    Term.(pure copts $ all_archive_sets $ archive_set)


(*
 * Commands
 *)


let default_cmd =
  let doc = "Manage dar backups." in
  let man = help_secs in
    Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
    Term.info "darckup" ~version:Conf.version ~sdocs:copts_sect ~doc ~man


let help_cmd =
  let doc = "Display help about darckup." in
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let man =
    [`S "DESCRIPTION";
     `P "Print help about darckup specific subjects."]
    @ help_secs
  in
    Term.(ret (pure help $ copts_t $ man_format $ choice_names $ topic)),
    Term.info "help" ~doc ~man


let list_cmd =
  let doc = "List all archive_sets." in
  let man =
    [`S "DESCRIPTION";
     `P "List all archive_sets that can be found in the configuration files."]
    @ help_secs
  in
    Term.(ret (pure list $ copts_t)),
    Term.info "list" ~sdocs:copts_sect ~doc ~man


let list_archive_cmd =
  let doc = "List all archives for an archive_set." in
  let aset_lst =
    let doc = "The archive_set to list." in
      Arg.(non_empty & pos_all string [] & info [] ~docv:"ARCHIVE_SET" ~doc)
  in
  let man =
    [`S "DESCRIPTION";
     `P "List all archives for an archive_set. The archive_set must exist in the
         configuration files."]
    @ help_secs
  in
    Term.(ret (pure list_archive $ copts_t $ aset_lst)),
    Term.info "list_archive" ~sdocs:copts_sect ~doc ~man


let create_cmd =
  let doc = "Create new archives for all selected archive_set." in
  let man =
    [`S "DESCRIPTION";
     `P "Invoke dar to create archive for the selected archive_sets."]
    @ help_secs
  in
    Term.(ret (pure create $ copts_t $ asopts_t)),
    Term.info "create" ~sdocs:copts_sect ~doc ~man


let clean_cmd =
  let doc = "Clean old archives for all selected archive_set." in
  let man =
    [`S "DESCRIPTION";
     `P "Cleanup old archives for the selected archive_sets."]
    @ help_secs
  in
    Term.(ret (pure clean $ copts_t $ asopts_t)),
    Term.info "clean" ~sdocs:copts_sect ~doc ~man


let cronjob_cmd =
  let doc = "Run 'create' and 'clean' as part of a cronjob." in
  let man =
    [`S "DESCRIPTION";
     `P "Invoke $(b,create) and $(b,clean) for the selected archive_sets."]
    @ help_secs
  in
    Term.(ret (pure cronjob $ copts_t $ asopts_t)),
    Term.info "cronjob" ~sdocs:copts_sect ~doc ~man


let cmds = [
  list_cmd;
  list_archive_cmd;
  create_cmd;
  clean_cmd;
  cronjob_cmd;
  help_cmd;
]


let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
