
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
