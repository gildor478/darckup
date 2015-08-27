
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

  let last t =
    failwith "Not implemented"

  let next t max_incremental prefix volumes =
    failwith "Not implemented"

  let pop t =
    failwith "Not implemented"

  let push t e =
    failwith "Not implemented"

  let list dn =
    of_filenames
      (List.map (Filename.concat dn) (Array.to_list (Sys.readdir dn)))
end
