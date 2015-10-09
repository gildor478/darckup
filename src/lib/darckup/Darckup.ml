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

module IntMap =
  Map.Make
    (struct
       type t = int
       let compare = (-)
     end)


module StringMap =
  Map.Make
    (struct
       type t = string
       let compare = String.compare
     end)


type filename = string
type volumes = filename IntMap.t

module Archive =
struct
  type kind = Full | Incremental of int
  type t =
      {
        short_prefix: string;
        volumes: volumes;
        kind: kind;
      }

  let is_full t = t.kind = Full

  let to_full_prefix t = t.short_prefix ^ "_full"

  let to_prefix t =
    match t.kind with
    | Full -> t.short_prefix ^ "_full"
    | Incremental n -> Printf.sprintf "%s_incr%02d" t.short_prefix n

  let to_filenames t = List.map snd (IntMap.bindings t.volumes)

  let re =
    let open Re in
      compile (seq [group (rep any); (* 1 *)
                    alt [str "_full";
                         seq [str "_incr";
                              opt (char '_');
                              group (rep1 digit)]]; (* 2 *)
                    char '.';
                    group (rep1 digit); (* 3 *)
                    str ".dar";
                    eol])

  let parse s =
    try
      let substr = Re.exec re s in
      let short_prefix = Re.get substr 1 in
      let volume = int_of_string (Re.get substr 3) in
      let kind =
        try
          Incremental (int_of_string (Re.get substr 2))
        with Not_found ->
          Full
      in
        {
          short_prefix;
          volumes = IntMap.add volume s IntMap.empty;
          kind = kind;
        }
    with Not_found ->
      invalid_arg s

  let merge t1 t2 =
    let open IntMap in
    if to_prefix t1 = to_prefix t2 then
      {t1 with
           volumes =
             merge
               (fun _ e1 e2 ->
                  match e1, e2 with
                  | Some fn1, Some fn2 ->
                      if fn1 <> fn2 then
                        invalid_arg fn2;
                      e1
                  | None, e | e, None -> e)
               t1.volumes t2.volumes}
    else
      invalid_arg (to_prefix t2)
end

module ArchiveSet =
struct
  module M =
    Map.Make
      (struct
         type t = string
         let compare = String.compare
       end)

  type t = Archive.t M.t

  let add a t =
    let open Archive in
    let k = to_prefix a in
      M.add k (try merge (M.find k t) a with Not_found -> a) t

  let remove a t =
    t (* TODO *)

  let of_filenames =
    List.fold_left
      (fun (t, bad) fn ->
         try
           add (Archive.parse fn) t, bad
         with Invalid_argument _ ->
           t, fn :: bad)
      (M.empty, [])

  let to_filenames t =
    List.rev
      (M.fold
         (fun _ a l -> List.rev_append (Archive.to_filenames a) l)
         t [])

  let length = M.cardinal

  let last t = snd (M.max_binding t)

  let next t max_incremental short_prefix =
    let open Archive in
    let make a =
      {a with volumes = IntMap.add 1 ((to_prefix a)^".1.dar") IntMap.empty}
    in
    let full =
      make
        {
          short_prefix;
          kind = Full;
          volumes = IntMap.empty;
        }
    in
    let a =
      if max_incremental <= 0 || length t = 0 then begin
        full
      end else begin
        match last t with
          | { kind = Full } as a ->
              make {a with kind = Incremental 1}
          | { kind = Incremental n } as a ->
              if n + 1 <= max_incremental then
                make {a with kind = Incremental (n + 1)}
              else
                full
      end
    in
      if not (length t = 0) && (last (add a t)) <> a then
        failwith
          (Printf.sprintf
             "Next archive %s will not be sorted as the last one, reset the \
              now_rfc3339 string to make sure the result will be sorted \
              after %s (the actual last one)"
             (Archive.to_prefix a) (Archive.to_prefix (last t)));
      a

  let rec pop t =
    let prefix, a = M.min_binding t in
    let t = M.remove prefix t in
      if not (M.is_empty t) && Archive.is_full a then begin
        match  snd (M.min_binding t) with
          | {Archive.kind = Archive.Incremental _} as a'
              when Archive.to_full_prefix a = Archive.to_full_prefix a' ->
              let t, a' = pop t in add a t, a'
          | _ ->
              t, a
      end else begin
        t, a
      end

  let rec npop n t =
    if n <= 0 then
      t, []
    else
      let t, a = pop t in
      let t, l = npop (n - 1) t in
        t, a :: l
end


type hooks = {
  pre_create_command: string option;
  post_create_command: string option;
  pre_clean_command: string option;
  post_clean_command: string option;
}


let default_hooks = {
  pre_create_command = None;
  post_create_command = None;
  pre_clean_command = None;
  post_clean_command = None;
}


type archive_set = {
  backup_dir: filename;
  darrc: filename;
  base_prefix: string;
  max_incrementals: int;
  max_archives: int;
  archive_set_hooks: hooks
}

type outf = string -> unit
type errf = string -> unit
type env = string array


type t = {
  dar: filename;
  now_rfc3339: string;
  dry_run: bool;
  ignore_glob_files: string list;
  global_hooks: hooks;
  archive_sets: (string * archive_set) list;

  (* System interface. *)
  command: unit Command.t -> string -> unit;
  exec: unit Command.t -> filename -> Command.arg list -> unit;
  readdir: filename -> filename array;
  remove: filename -> unit;
  getcwd: unit -> filename;
  file_exists: filename -> bool;
  is_directory: filename -> bool;
  log: [`Debug | `Info | `Warning | `Error] -> string -> unit;
}


let default =
  {
    dar = "dar";
    now_rfc3339 =
      begin
        let open CalendarLib in
          Printer.Calendar.sprint "%FT%T%:z" (Calendar.now ())
      end;
    dry_run = false;
    global_hooks = default_hooks;
    ignore_glob_files = [];
    archive_sets = [];

    command = Command.command;
    exec = Command.exec;
    readdir = Sys.readdir;
    remove = Sys.remove;
    getcwd = Sys.getcwd;
    file_exists = Sys.file_exists;
    is_directory = Sys.is_directory;
    log = (fun _ _ -> ());
  }


let logf t lvl fmt = Printf.ksprintf (t.log lvl) fmt


let load_one_configuration t fn =
  let () = logf t `Info "Loading configuration file %s." fn in
  let ini =
    try
      new Inifiles.inifile fn
    with
    | Inifiles.Ini_parse_error (lineno, fn) ->
        failwith
          (Printf.sprintf
             "Parse error line %d, file '%s'" lineno fn)
  in

  let get ?default parse sect var =
    try
      let str  = ini#getval sect var in
        try
          parse str
        with
        | Failure msg ->
          failwith
            (Printf.sprintf
               "Cannot parse '%s > %s = %s': %s" sect var str msg)
        | e ->
          failwith
            (Printf.sprintf
               "Cannot parse '%s > %s = %s': %s"
               sect var str (Printexc.to_string e))
    with
    | Inifiles.Invalid_element _ ->
      begin
        match default with
        | Some d -> d
        | None ->
            failwith
              (Printf.sprintf
                 "Missing '%s -> %s' attribute in configuration file %S."
                 sect var fn)
      end
  in

  let to_absolute fn dn =
    if FilePath.is_relative fn then
      FilePath.make_absolute dn fn
    else
      fn
  in

  let dn = Filename.dirname (to_absolute fn (t.getcwd ())) in

  let directory_exists fn =
    let fn' = to_absolute fn dn in
      if t.file_exists fn' && t.is_directory fn' then
        fn'
      else
        failwith (Printf.sprintf "directory '%s' doesn't exist" fn')
  in

  let file_exists fn =
    let fn' = to_absolute fn dn in
      if t.file_exists fn' && not (t.is_directory fn') then
        fn'
      else
        failwith (Printf.sprintf "file '%s' doesn't exist" fn')
  in

  let integer_with_min min s =
    let i = int_of_string s in
      if i >= min then
        i
      else
        failwith (Printf.sprintf "%d is too small (min. %d)" i min)
  in

  let comma_list f s =
    let re_comma =
      Re.compile
        (let open Re in seq [rep (char ' '); char ','; rep (char ' ')])
    in
      List.map f (Re_pcre.split ~rex:re_comma s)
  in

  let identity s = s in

  let opt f s = Some (f s) in

  let re_archive =
    let open Re in compile (seq [str "archive_set:"; group (rep1 any); eol])
  in

  let parse_hooks sect hks =
    let get dflt fname = get ~default:dflt (opt identity) sect fname in
      {
        pre_create_command = get hks.pre_create_command "pre_create_command";
        post_create_command = get hks.post_create_command "post_create_command";
        pre_clean_command = get hks.pre_clean_command "pre_clean_command";
        post_clean_command = get hks.post_clean_command "post_clean_command";
      }
  in

  let t =
    List.fold_left
      (fun t sect ->
         if sect = "default" then begin
           {
             t with
                 dar = get ~default:t.dar identity sect "dar";
                 global_hooks = parse_hooks sect t.global_hooks;
                 ignore_glob_files =
                   get
                     ~default:t.ignore_glob_files
                     (comma_list identity)
                     sect
                     "ignore_glob_files";
           }
         end else begin
           try
             let substr = Re.exec re_archive sect in
             let aname = Re.get substr 1 in
             let aset =
               try
                 List.assoc aname t.archive_sets
               with Not_found ->
                 {
                   backup_dir = get directory_exists sect "backup_dir";
                   darrc = get file_exists sect "darrc";
                   base_prefix = get identity sect "base_prefix";
                   archive_set_hooks = default_hooks;
                   max_incrementals = 6;
                   max_archives = 30;
                 }
             in
             let aset =
               {
                 backup_dir =
                   get ~default:aset.backup_dir directory_exists
                     sect "backup_dir";
                 darrc =
                   get ~default:aset.darrc file_exists
                     sect "darrc";
                 base_prefix =
                   get ~default:aset.base_prefix identity
                     sect "base_prefix";
                 archive_set_hooks =
                   parse_hooks sect aset.archive_set_hooks;
                 max_incrementals =
                   get ~default:aset.max_incrementals (integer_with_min 0)
                     sect "max_incrementals";
                 max_archives =
                   get ~default:aset.max_archives (integer_with_min 0)
                     sect "max_archives";
               }
             in
             let rec replace_append =
               function
                 | [] -> [aname, aset]
                 | (aname', _) :: tl when aname' = aname -> (aname, aset) :: tl
                 | e :: tl -> e :: replace_append tl
             in
               {t with archive_sets = replace_append t.archive_sets}
           with Not_found ->
             failwith
               (Printf.sprintf "Don't know what to do with section '%s'." sect)
         end
      )
      t ini#sects
  in
    {t with archive_sets = t.archive_sets}


let load_configuration t ?dir fn =
  let lst =
    fn ::
    match dir with
    | Some dn when t.file_exists dn && t.is_directory dn ->
        let arr = t.readdir dn in
          Array.sort String.compare arr;
          Array.fold_right
            (fun fn lst ->
               if Filename.check_suffix fn ".ini" then
                 Filename.concat dn fn :: lst
               else
                 lst)
            arr []
    | _ -> []
  in
  let t =
    List.fold_left
      (fun t fn ->
         if t.file_exists fn then
           load_one_configuration t fn
         else
           t)
      t lst
  in
    logf t `Info "Hello!";
    List.iter
      (fun (aname, _) -> logf t `Info "archive_set found: %s" aname)
      t.archive_sets;
    t


let load_archive_sets t =
  (* Cons the value v to the list, using [v] if not yet set. *)
  let cons s v mp =
    let l = try StringMap.find s mp with Not_found -> [] in
      StringMap.add s (v :: l) mp
  in

  (* Group archive per backup_dir. *)
  let group_backup_dir =
    List.fold_left
      (fun mp ((_, {backup_dir = dn}) as e) -> cons dn e mp)
      StringMap.empty t.archive_sets
  in

  let is_ignored =
    let open Re in
    let re =
      compile (alt (List.map Re_glob.glob t.ignore_glob_files))
    in
      execp re
  in

  let process_backup_dir backup_dir archive_sets map_names =
    (* We create on Re.group per archive_set and we use to know what
     * archive_set matches the given filename.
     *)
    let files_lists = Array.make (List.length archive_sets) [] in
    let classifier_re =
       let open Re in
       let groups =
         List.map
           (fun (_, aset) -> group (seq [str aset.base_prefix; rep any]))
           archive_sets
       in
         compile (seq [alt groups; rep any; str ".dar"; eol])
    in
    let test_assign bn =
      if not (is_ignored bn) then begin
        (* Test if the basename match and cons the filename if yes. *)
        try
          let substr = Re.exec classifier_re bn in
          let found = ref false in
          let i = ref 0 in
            while not !found && !i < Array.length files_lists do
              if Re.test substr (!i + 1) then begin
                files_lists.(!i) <-
                  Filename.concat backup_dir bn :: files_lists.(!i);
                found := true
              end;
              incr i
            done
        with Not_found ->
            logf t `Warning "File %S doesn't match any archive."
            (Filename.concat backup_dir bn)
      end
    in
    let _, map_names =
      (* Group filenames from the backup directory per known prefix. *)
      Array.iter test_assign (t.readdir backup_dir);
      List.fold_left
        (fun (i, map_names) (aname, aset) ->
           let archv, lst =
             List.iter
               (fun fn ->
                 logf t `Info "Adding file %S to archive %S." fn aname)
               files_lists.(i);
             ArchiveSet.of_filenames files_lists.(i)
           in
             List.iter
               (fun fn ->
                 logf t `Warning "File %S doesn't match archive %S." fn aname)
               lst;
              i + 1, StringMap.add aname archv map_names)
        (0, map_names) archive_sets
    in
      map_names
  in

  let map_names =
    StringMap.fold process_backup_dir group_backup_dir StringMap.empty
  in
    List.map
      (fun (aname, aset) -> aname, aset, StringMap.find aname map_names)
      t.archive_sets


let command_default t =
  let tdry_run = t.dry_run in
  let open Command in
  {default with
       outf = logf t `Info "%s";
       errf = logf t `Warning "%s";
       dry_run = tdry_run}


type variable = string
let variables =
  [
    "current.archive_set.name",
    `CurrentArchiveSet
      (fun _ aname _ _ -> aname),
    "name of archive_set currently processed.";

    "current.archive.prefix",
    `CurrentArchive
      (fun _ archv -> Archive.to_prefix archv),
    "archive prefix of the archive currently processed.";

    "all_archive_sets",
    `All (fun t -> (*TODO *) ""),
    "all available archive_sets.";
  ]


let hook_variables =
  List.map (fun (var, _, help) -> (var, help)) variables


let map_variables =
  List.fold_left
    (fun mp (nm, f, _) ->
       if StringMap.mem nm mp then
         failwith
           (Printf.sprintf "Darckup variable %s is already defined." nm);
       StringMap.add nm f mp)
    StringMap.empty variables


let run_hook t ?with_archive_set ?with_archive fname cmd_opt =
  let id =
    match with_archive_set with
    | Some (aname, aset, _) ->
        Printf.sprintf "archive_set:%s->%s" aname fname
    | None ->
        Printf.sprintf "default->%s" fname
  in

  let subst t cmd var =
    let f =
      try
        StringMap.find var map_variables
      with Not_found ->
        failwith
          (Printf.sprintf
             "Unknown variable %s in command %S for %s." var cmd id)
    in
    let na () =
      failwith
        (Printf.sprintf
           "Variable %s is not available in command %S for %s." var cmd id)
    in
       match f, with_archive_set, with_archive with
       | `CurrentArchiveSet f, Some (aname, aset, archv_set), _ ->
           f t aname aset archv_set
       | `CurrentArchiveSet _, None, _ ->
           na ()
       | `CurrentArchive f, _, Some archv ->
           f t archv
       | `CurrentArchive f, _, None ->
           na ()
       | `All f, _, _ ->
           f t
  in

  match cmd_opt with
  | Some s ->
      begin
        let cmd =
          let b = Buffer.create (String.length s) in
            Buffer.add_substitute b (subst t s) s;
            Buffer.contents b
        in
          fun () ->
            logf t `Info "Running command %S for %s (dry_run: %b)."
              cmd id t.dry_run;
            t.command (command_default t) cmd
      end
  | None ->
      fun () -> logf t `Info "No command defined for %s." id


let create t =
  let lst =
    List.map
      (fun (aname, aset, archv_set) ->
         let archv =
           ArchiveSet.next archv_set aset.max_incrementals
             (Filename.concat
                aset.backup_dir (aset.base_prefix ^ "_" ^ t.now_rfc3339))
         in
         let dar_args =
           let open Command in
             [A "-c"; Fn (Archive.to_prefix archv); A "-noconf";
              A "-B"; Fn aset.darrc]
             @ (if not (Archive.is_full archv) then
                  [A "-A"; Fn (Archive.to_full_prefix archv)]
                else
                  [])
         in
         let pre_create_command  =
           run_hook t
             ~with_archive_set:(aname, aset, archv_set)
             ~with_archive:archv
             "pre_create_command"
             aset.archive_set_hooks.pre_create_command
         in
         let archv_set = ArchiveSet.add archv archv_set in
         let post_create_command =
           run_hook t
             ~with_archive_set:(aname, aset, archv_set)
             ~with_archive:archv
             "post_create_command"
             aset.archive_set_hooks.post_create_command
         in
           (aname, archv), pre_create_command, dar_args, post_create_command)
      (load_archive_sets t)
  in
  let pre_cmd =
    run_hook t "pre_create_command" t.global_hooks.pre_create_command
  in
  let post_cmd =
    run_hook t "post_create_command" t.global_hooks.post_create_command
  in

  (* Invoke dar for each archive_set. *)
  pre_cmd ();
  List.iter
    (fun ((aname, _), pre_cmd, dar_args, post_cmd) ->
       pre_cmd ();
       logf t `Info "Creating dar archive with command %S."
         (Command.string_of_exec t.dar dar_args);
       t.exec (command_default t) t.dar dar_args;
       post_cmd ())
    lst;
  post_cmd ();

  List.map (fun (fst, _, _, _) -> fst) lst


let clean t =
  let clean_one_archive_set (aname, aset, archv_set) =
    let d = ArchiveSet.length archv_set - aset.max_archives in
      logf t `Info
        "Cleaning archive_set:%s: %d archives present, will remove %d of them."
        aname (ArchiveSet.length archv_set) d;
      if d > 0 then begin
        let _, lst =
          List.fold_left
            (fun (archv_set, lst) archv ->
               let pre_clean_command =
                 run_hook t
                   ~with_archive_set:(aname, aset, archv_set)
                   ~with_archive:archv
                   "pre_clean_command"
                   aset.archive_set_hooks.pre_clean_command
               in
               let archv_set = ArchiveSet.remove archv archv_set in
               let post_clean_command =
                 run_hook t
                   ~with_archive_set:(aname, aset, archv_set)
                   ~with_archive:archv
                   "post_clean_command"
                   aset.archive_set_hooks.post_clean_command
               in
               let filenames = Archive.to_filenames archv in
                 archv_set,
                 (aname, pre_clean_command, filenames,
                  post_clean_command) :: lst)
            (archv_set, []) (snd (ArchiveSet.npop d archv_set))
        in
          lst
      end else begin
        []
      end
  in
  let lst =
    List.flatten (List.map clean_one_archive_set (load_archive_sets t))
  in
  let pre_cmd =
    run_hook t "pre_clean_command" t.global_hooks.pre_clean_command
  in
  let post_cmd =
    run_hook t "post_clean_command" t.global_hooks.post_clean_command
  in

    pre_cmd ();
    List.iter
      (fun (aname, pre_cmd, fn_lst, post_cmd) ->
         pre_cmd ();
         List.iter
           (fun fn ->
              logf t `Info "Removing file %s (cleaning of archive %s)"
                fn aname;
              if not t.dry_run then
                t.remove fn)
           fn_lst;
         post_cmd ())
      lst;
    post_cmd ()
