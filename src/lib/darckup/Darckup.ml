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

type archive_set = {
  backup_dir: filename;
  darrc: filename;
  base_prefix: string;
  max_incrementals: int;
  max_archives: int;
  pre_create_command: string option;
  post_create_command: string option;
  pre_clean_command: string option;
  post_clean_command: string option;
}

type outf = string -> unit
type errf = string -> unit
type env = string array

type t = {
  dar: filename;
  now_rfc3339: string;
  dry_run: bool;
  ignore_glob_files: string list;
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

  let t =
    List.fold_left
      (fun t sect ->
         if sect = "default" then begin
           {
             t with
                 dar = get ~default:t.dar identity sect "dar";
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
               {
                 backup_dir = get directory_exists sect "backup_dir";
                 darrc = get file_exists sect "darrc";
                 base_prefix = get identity sect "base_prefix";
                 max_incrementals =
                   get
                     ~default:6
                     (integer_with_min 0)
                     sect
                     "max_incrementals";
                 max_archives =
                   get
                     ~default:30
                     (integer_with_min 0)
                     sect
                     "max_archives";
                 pre_create_command =
                   get
                     ~default:None
                     (opt identity)
                     sect
                     "pre_create_command";
                 post_create_command =
                   get
                     ~default:None
                     (opt identity)
                     sect
                     "post_create_command";
                 pre_clean_command =
                   get
                     ~default:None
                     (opt identity)
                     sect
                     "pre_clean_command";
                 post_clean_command =
                   get
                     ~default:None
                     (opt identity)
                     sect
                     "post_clean_command";
               }
             in
               {t with archive_sets = (aname, aset) :: t.archive_sets}
           with Not_found ->
             failwith
               (Printf.sprintf "Don't know what to do with section '%s'." sect)
         end
      )
      t ini#sects
  in
    {t with archive_sets = List.rev t.archive_sets}


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
    List.fold_left
      (fun t fn ->
         if t.file_exists fn then
           load_one_configuration t fn
         else
           t)
      t lst

let load_archive_sets t =
  let module MapString =
    Map.Make
      (struct
         type t = filename
         let compare = String.compare
       end)
  in

  (* Cons the value v to the list, using [v] if not yet set. *)
  let cons s v mp =
    let l = try MapString.find s mp with Not_found -> [] in
      MapString.add s (v :: l) mp
  in

  (* Group archive per backup_dir. *)
  let group_backup_dir =
    List.fold_left
      (fun mp ((_, {backup_dir = dn}) as e) -> cons dn e mp)
      MapString.empty t.archive_sets
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
              i + 1, MapString.add aname archv map_names)
        (0, map_names) archive_sets
    in
      map_names
  in

  let map_names =
    MapString.fold process_backup_dir group_backup_dir MapString.empty
  in
    List.map
      (fun (aname, aset) -> aname, aset, MapString.find aname map_names)
      t.archive_sets


let create t =
  let command_default =
    let tdry_run = t.dry_run in
    let open Command in
    {default with
         outf = logf t `Info "%s";
         errf = logf t `Warning "%s";
         dry_run = tdry_run}
  in

  let command_opt fld =
    function
    | Some cmd ->
        logf t `Info "Running command %S from field %s." cmd fld;
        t.command command_default cmd
    | None ->
        logf t `Info "No command defined from field %s." fld
  in

  let lst =
    List.map
      (fun (aname, aset, archv) ->
         let a =
           ArchiveSet.next archv aset.max_incrementals
             (Filename.concat
                aset.backup_dir (aset.base_prefix ^ "_" ^ t.now_rfc3339))
         in
         let cmd_subst s_opt =
           match s_opt with
             | Some s ->
                 let b = Buffer.create (String.length s) in
                   Buffer.add_substitute b
                     (function
                        | "current.last.prefix" ->
                            Filename.quote (Archive.to_prefix a)
                        | id ->
                            failwith
                              (Printf.sprintf
                                 "Unknown identifier %S in command %S."
                                 id s))
                     s;
                   Some (Buffer.contents b)
             | None -> None
         in
         let dar_args =
           let open Command in
             [A "-c"; Fn (Archive.to_prefix a); A "-noconf";
              A "-B"; Fn aset.darrc]
             @ (if not (Archive.is_full a) then
                  [A "-A"; Fn (Archive.to_full_prefix a)]
                else
                  [])
         in
           (aname, a),
           cmd_subst aset.pre_create_command,
           dar_args,
           cmd_subst aset.post_create_command)
      (load_archive_sets t)
  in

  (* Invoke dar for each archive_set. *)
  List.iter
    (fun ((aname, _), pre_cmd, dar_args, post_cmd) ->
          command_opt
            (Printf.sprintf "archive_set:%s->pre_create_command" aname)
            pre_cmd;
          begin
            logf t `Info "Creating dar archive with command %S."
              (Command.string_of_exec t.dar dar_args);
            t.exec command_default t.dar dar_args
          end;
          command_opt
            (Printf.sprintf "archive_set:%s->post_create_command" aname)
            post_cmd)
    lst;

  List.map (fun (fst, _, _, _) -> fst) lst


let clean t =
  failwith "Not implemented"


type variable = string


let getenv t ?current_archive_set variable =
  failwith "Not implemented"


let getenv_variables =
  ["current.name", "name of archive_set currently processed.";
   "current.filenames", "list of all files for the current archive_set.";
   "current.last.prefix", "archive prefix of the last created archive.";
   "all_archive_sets",  "all available archive_sets."]

