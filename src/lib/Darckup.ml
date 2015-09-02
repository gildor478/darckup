
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
        prefix: string;
        volumes: volumes;
        kind: kind;
      }

  let is_full t = t.kind = Full

  let to_filenames t = List.map snd (IntMap.bindings t.volumes)

  let re =
    let open Re in
      compile (seq [group (rep any); (* 1 *)
                    alt [str "full";
                         seq [str "incr";
                              opt (char '_');
                              group (rep1 digit)]]; (* 2 *)
                    char '.';
                    group (rep1 digit); (* 3 *)
                    str ".dar";
                    eol])

  let parse s =
    try
      let substr = Re.exec re s in
      let prefix = Re.get substr 1 in
      let volume = int_of_string (Re.get substr 3) in
      let kind =
        try
          Incremental (int_of_string (Re.get substr 2))
        with Not_found ->
          Full
      in
        {
          prefix = prefix;
          volumes = IntMap.add volume s IntMap.empty;
          kind = kind;
        }
    with Not_found ->
      invalid_arg s
end

module ArchiveSet =
struct
  module M =
    Map.Make
      (struct
         type t = string * Archive.kind
         let compare = Pervasives.compare
       end)

  type t = Archive.t M.t

  let add a t =
    let k = a.Archive.prefix, a.Archive.kind in
    let a' =
      try
        let a' = M.find k t in
          {a' with
            Archive.volumes =
              IntMap.merge
                (fun _ e1 e2 ->
                   match e1, e2 with
                     | Some fn1, Some fn2 ->
                         if fn1 <> fn2 then
                           invalid_arg fn2;
                         e1
                     | None, e | e, None -> e)
                a'.Archive.volumes a.Archive.volumes}
      with Not_found ->
        a
    in
      M.add k a' t

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

  let next t max_incremental prefix_full =
    if max_incremental <= 0 then begin
      prefix_full ^ "full"
    end else begin
      let open Archive in
      match last t with
        | { kind = Full } as a -> a.prefix ^ "incr01"
        | { kind = Incremental n } as a ->
            if n + 1 <= max_incremental then
              Printf.sprintf "%sincr%02d" a.prefix (n + 1)
            else
              prefix_full ^ "full"
    end

  let rec pop t =
    let (pre, knd) as key, a = M.min_binding t in
    let t = M.remove key t in
      if not (M.is_empty t) && knd = Archive.Full then begin
        match  fst (M.min_binding t) with
          | pre', Archive.Incremental _ when pre = pre' ->
              let t, a' = pop t in add a t, a'
          | _, _ ->
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


let default = {
    dar = "dar";
    now_rfc3339 = "TODO"; (* TODO *)
    pre_command = None;
    post_command = None;
    ignore_files = [];
    archive_sets = [];

    command = Sys.command;
    readdir = Sys.readdir;
    remove = Sys.remove;
    getcwd = Sys.getcwd;
    file_exists = Sys.file_exists;
    is_directory = Sys.is_directory;
    log = (fun _ _ -> ());
}


let load t fn =
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
      FilePath.make_absolute fn dn
    else
      fn
  in

  let dn = to_absolute fn (t.getcwd ()) in

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
    let open Re in compile (seq [str "archive:"; group (rep1 any); eol])
  in

  let t =
    List.fold_left
      (fun t sect ->
         if sect = "default" then begin
           {
             t with
                 dar = get ~default:t.dar identity sect "dar";
                 pre_command =
                   get
                     ~default:t.pre_command
                     (opt identity)
                     sect
                     "pre_command";
                 post_command =
                   get
                     ~default:t.post_command
                     (opt identity)
                     sect
                     "post_command";
                 ignore_files =
                   get
                     ~default:t.ignore_files
                     (comma_list identity)
                     sect
                     "ignore_files";
           }
         end else begin
           try
             let substr = Re.exec re_archive sect in
             let aname = Re.get substr 1 in
             let aset =
               {
                 backup_dir = get directory_exists sect "backup_dir";
                 darrc = get file_exists sect "darrc";
                 prefix = get identity sect "prefix";
                 max_incrementals = get ~default:6 (integer_with_min 0) sect
                                      "max_incrementals";
                 max_archives = get ~default:30 (integer_with_min 0) sect
                                  "max_archives";
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


let create t =
  failwith "Not implemented"


let clean t =
  failwith "Not implemented"
