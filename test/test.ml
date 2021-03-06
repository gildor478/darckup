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

open OUnit2
open FileUtil
open Darckup


let darckup_exec = Conf.make_exec "darckup"
let dar_exec = Conf.make_exec "dar"


module StringListDiff =
OUnitDiff.ListSimpleMake
  (struct
     type t = string
     let pp_printer fmt = Format.fprintf fmt "%S"
     let compare = String.compare
     let pp_print_sep = OUnitDiff.pp_comma_separator
   end)


module StringSetDiff =
OUnitDiff.SetMake
  (struct
     type t = string
     let pp_printer fmt = Format.fprintf fmt "%S"
     let compare = String.compare
     let pp_print_sep = OUnitDiff.pp_comma_separator
   end)


let write_file test_ctxt binding fn cnt =
  let open FilePath in
  let open FileUtil in

  let cnt =
    (* Remove blanks at beginning of all lines. *)
    let b = Buffer.create (String.length cnt) in
    let eol = ref true in
      String.iter
        (function
           | '\n' ->
               eol := true;
               Buffer.add_char b '\n'
           | ' ' when !eol ->
               ()
           | c ->
               eol := false;
               Buffer.add_char b c)
        cnt;
      Buffer.contents b
  in

  let b =
    let b = Buffer.create (String.length cnt) in
      Buffer.add_substitute b
        (fun keyword ->
           try
             List.assoc keyword binding
           with Not_found ->
             failwith
               (Printf.sprintf "unknown substitution variable %S" keyword))
        cnt;
      b
  in

  let () = mkdir ~parent:true (dirname fn) in
  let fd = open_out fn in
    Buffer.output_buffer fd b;
    close_out fd;
    logf test_ctxt
      `Info "Creating file %s with content:\n%s" fn (Buffer.contents b)


let assert_equal_dir_list lst dn =
  StringSetDiff.assert_equal
    (StringSetDiff.of_list lst)
    (StringSetDiff.of_list (Array.to_list (Sys.readdir dn)))


let assert_bigger_size fn1 fn2 =
  let open FileUtil in
    assert_equal
      ~printer:(fun s -> string_of_size s)
      ~cmp:(fun sz1 sz2 -> size_compare sz1 sz2 > 0)
      (stat fn1).size (stat fn2).size


let assert_equal_next exp (got_archv, got_opt_ref_archv) =
  assert_equal
    ~msg:"ArchiveSet.next"
    ~printer:(fun (s, os) ->
                s^", "^match os with None -> "<none>" | Some s -> s)
    exp
    (Archive.to_prefix got_archv,
     match got_opt_ref_archv with
     | None -> None
     | Some archv -> Some (Archive.to_prefix archv))


let test_archive_set _ =
  let files =
      [
        "foobar_20150831_full.1.dar";
        "foobar_20150831_incr_1.1.dar";
        "foobar_20150831_incr_2.1.dar";
        "foobar_20150831_incr_3.1.dar";
        "foobar_20150831_incr_4.1.dar";
        "foobar_20150831_incr_4.2.dar";
        "foobar_20150905_full.1.dar";
        "foobar_20150905_incr1.1.dar";
      ]
  in
  let set, bad =
    (* of_filenames *)
    ArchiveSet.of_filenames (List.rev files) in
    StringListDiff.assert_equal [] bad;
    StringListDiff.assert_equal files (ArchiveSet.to_filenames set);

    (* length *)
    assert_equal
      ~msg:"ArchiveSet.length"
      ~printer:string_of_int
      7
      (ArchiveSet.length set);

    (* last *)
    StringListDiff.assert_equal
      ["foobar_20150905_incr1.1.dar"]
      (Archive.to_filenames (ArchiveSet.last set));

    (* pop *)
    StringListDiff.assert_equal
      ["foobar_20150831_incr_1.1.dar"]
      (Archive.to_filenames (snd (ArchiveSet.pop set)));

    (* npop *)
    StringListDiff.assert_equal
      [
        "foobar_20150831_incr_1.1.dar";
        "foobar_20150831_incr_2.1.dar";
        "foobar_20150831_incr_3.1.dar";
        "foobar_20150831_incr_4.1.dar";
        "foobar_20150831_incr_4.2.dar";
        "foobar_20150831_full.1.dar";
        "foobar_20150905_incr1.1.dar";
        "foobar_20150905_full.1.dar";
      ]
      (List.flatten
         (List.map (fun lst -> Archive.to_filenames lst)
            (snd (ArchiveSet.npop (ArchiveSet.length set) set))));

    (* next *)
    assert_equal_next
      ("foobar_20150907_full", None)
      (ArchiveSet.next set false (Some 1) "foobar_20150907");
    assert_equal_next
      ("foobar_20150905_incr00002", Some "foobar_20150905_full")
      (ArchiveSet.next set false (Some 2) "foobar_20150907");
    begin
      try
        let _, _ = ArchiveSet.next set false (Some 1) "foobar_20150904" in
          assert_failure
            "ArchiveSet.next create an archive that will not be the \
             last one."
      with Failure _ ->
        ()
    end;

    ()


let test_archive_can_include_done _ =
  let all_files =
    [
      "foobar_20150831_full.1.dar";
      "foobar_20150831_full_catalog.1.dar.done";
      "foobar_20150831_incr_1_catalog.1.dar.done";
      "foobar_20150831_incr_2.1.dar";
      "foobar_20150831_incr_3.1.dar";
      "foobar_20150831_incr_4.1.dar";
      "foobar_20150831_incr_4.2.dar";
      "foobar_20150905_full.1.dar";
      "foobar_20150905_incr1.1.dar";
    ]
  in
  let set, bad = ArchiveSet.of_filenames all_files in
  StringListDiff.assert_equal [] bad;
  StringSetDiff.assert_equal
    (StringSetDiff.of_list all_files)
    (StringSetDiff.of_list (ArchiveSet.to_filenames set))


let test_archive_can_use_only_done _ =
  let all_files =
    [
      "foobar_20150831_full_catalog.1.dar";
      "foobar_20150831_full_catalog.1.dar.done";
      "foobar_20150831_incr_1_catalog.1.dar";
      "foobar_20150831_incr_1_catalog.1.dar.done";
    ]
  in
  let set, bad = ArchiveSet.of_filenames all_files in
  StringListDiff.assert_equal [] bad;
  StringSetDiff.assert_equal
    (StringSetDiff.of_list all_files)
    (StringSetDiff.of_list (ArchiveSet.to_filenames set))


let test_archive_only_catalogs_doesnt_contain_done _ =
  let all_files =
    [
      "foobar_20150831_full.1.dar";
      "foobar_20150831_full_catalog.1.dar";
      "foobar_20150831_full_catalog.1.dar.done";
    ]
  in
  let set, bad = ArchiveSet.of_filenames all_files in
  StringListDiff.assert_equal [] bad;
  StringListDiff.assert_equal
    ["foobar_20150831_full_catalog.1.dar"]
    (ArchiveSet.to_filenames ~catalogs:true set)


let test_archive_only_volumes_doesnt_contain_done _ =
  let all_files =
    [
      "foobar_20150831_full.1.dar";
      "foobar_20150831_full_catalog.1.dar";
      "foobar_20150831_full_catalog.1.dar.done";
    ]
  in
  let set, bad = ArchiveSet.of_filenames all_files in
  StringListDiff.assert_equal [] bad;
  StringListDiff.assert_equal
    ["foobar_20150831_full.1.dar"]
    (ArchiveSet.to_filenames ~volumes:true set)


let test_archive_set_transition _ =
  let files =
      [
        "foobar_20150831_full.1.dar";
        "foobar_20150831_incr_1.1.dar";
        "foobar_20150831_incr_2.1.dar";
        "foobar_20150831_incr_3.1.dar";
        "foobar_20150831_incr_4.1.dar";
        "foobar_20150831_incr_99.1.dar";
      ]
  in
  (* of_filenames *)
  let set, bad = ArchiveSet.of_filenames (List.rev files) in
  let () =
    StringListDiff.assert_equal [] bad;
    StringListDiff.assert_equal files (ArchiveSet.to_filenames set);
    assert_equal_next
      ("foobar_20150831_incr00100", Some "foobar_20150831_full")
      (ArchiveSet.next set false None "foobar_20150907")
  in
  let files' = files @ ["foobar_20150831_incr_00100.1.dar"] in
  let set', bad' = ArchiveSet.of_filenames (List.rev files') in
    StringListDiff.assert_equal [] bad';
    StringListDiff.assert_equal files' (ArchiveSet.to_filenames set');
    assert_equal_next
      ("foobar_20150831_incr00101", Some "foobar_20150831_full")
      (ArchiveSet.next set' false None "foobar_20150907")


module T =
struct
  let create test_ctxt in_tmpdir =
    let t =
      {
        default with
            dar = dar_exec test_ctxt;
            now_rfc3339 = "20150926";
            log =
              (fun lvl s ->
                 begin
                   match lvl with
                   | `Debug -> ()
                   | (`Info | `Warning | `Error) as lvl' ->
                       logf test_ctxt lvl' "%s" s
                 end;
                 if lvl = `Error || lvl = `Warning then
                   failwith s);
      }
    in
    let t =
      load_configuration t
        ~dir:(in_tmpdir ["etc"; "darckup.ini.d"])
        (in_tmpdir ["etc"; "darckup.ini"])
    in
      ref t

  let set_now_rfc3339 rt now_rfc3339 =
    rt := {!rt with now_rfc3339};
    !rt
end


let setup_filesystem test_ctxt =
  let tmpdir = bracket_tmpdir test_ctxt in
  let in_tmpdir lst = FilePath.make_filename (tmpdir :: lst) in
  let write_file lst cnt =
    write_file test_ctxt ["tmpdir", tmpdir] (in_tmpdir lst) cnt
  in
    (* Populate the filesystem. *)
    mkdir ~parent:true (in_tmpdir ["var"; "foobar"]);
    touch (in_tmpdir ["var"; "foobar"; "01.txt"]);
    write_file ["var"; "foobar"; "02.txt"] (String.make 1512 'x');
    mkdir ~parent:true (in_tmpdir ["var"; "barbaz"]);
    touch (in_tmpdir ["var"; "barbaz"; "01.txt"]);
    touch (in_tmpdir ["var"; "barbaz"; "02.txt"]);
    mkdir ~parent:true (in_tmpdir ["srv"; "backup"]);
    (* Return the useful functions. *)
    tmpdir, in_tmpdir, write_file


let create_ignore_result t =
  let _lst: (string * Archive.t) list = create t in ()


module Examples =
struct
  let darrc =
    "create:
     -w
     -D
     -z
     -aa
     -ac
     -g var/foobar
     -R ${tmpdir}
     all:
     -Z *.bz2
     -Z *.zip
      "
  let default =
      "[default]
       ignore_glob_files=*.md5sums,*.done
       post_create_command=touch \"${tmpdir}/sync-done\"
       post_clean_command=rm -f \"${tmpdir}/sync-done\"
      "
  let archive_set_foobar =
      "[archive_set:foobar]
       backup_dir=${tmpdir}/srv/backup
       darrc=${tmpdir}/etc/foobar.darrc
       base_prefix=foobar
       max_incrementals=1
       max_archives=3
      "

  let archive_set_barbaz =
      "[archive_set:barbaz]
       backup_dir=${tmpdir}/srv/backup
       darrc=../barbaz.darrc
       base_prefix=barbaz
       max_incrementals=1
       max_archives=3
      "
end


let rec comb p lst acc =
  match lst with
    | hd :: tl ->
        List.fold_left
          (fun acc p -> comb p tl acc)
          acc
          (List.rev_map (fun s -> p ^ s) hd)
    |   [] ->
        p :: acc


let test_load_clean_create test_ctxt =
  let tmpdir, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let create_current_files =
    comb "foobar_201509"
      [ ["26"; "29"]; ["_full"; "_incr00001"; "_incr00002"]; [".1.dar"; ".done"]]
      (comb "barbaz_201509"
         [["26"; "28"; "30"]; ["_full"; "_incr00001"]; [".1.dar"]] [])
  in
  let clean_current_files =
    comb "foobar_201509"
      [ ["29"]; ["_full"; "_incr00001"; "_incr00002"]; [".1.dar"; ".done"]]
      (comb "barbaz_201509"
         [["30"]; ["_full"; "_incr00001"]; [".1.dar"]]
         ["barbaz_20150928_full.1.dar"])
  in
  let () =
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"] Examples.default;
    write_file ["etc"; "darckup.ini.d"; "00foobar.ini"]
      (Examples.archive_set_foobar
       ^"post_create_command=touch \\${current.archive.prefix}.done
         pre_clean_command=rm \\${current.archive.prefix}.done
        ");
    write_file ["etc"; "darckup.ini.d"; "01foobar.ini"]
      "[archive_set:foobar]
       max_incrementals=2
      ";
    write_file ["etc"; "darckup.ini.d"; "02barbaz.ini"]
      Examples.archive_set_barbaz;
    write_file ["etc"; "foobar.darrc"] Examples.darrc;
    write_file ["etc"; "barbaz.darrc"] Examples.darrc
  in
  let afoobar t = List.assoc "foobar" t.archive_sets in
  let t = T.create test_ctxt in_tmpdir in
    (* Check result of loading INI file. *)
    StringListDiff.assert_equal
      ["*.md5sums"; "*.done"]
      !t.ignore_glob_files;
    StringListDiff.assert_equal
      ["foobar"; "barbaz"]
      (List.map fst !t.archive_sets);
    assert_equal ~printer:(function Some s -> s | None -> "none")
      (Some ("touch \"" ^ tmpdir ^ "/sync-done\""))
      !t.global_hooks.post_create_command;
    assert_equal ~printer:(function Some s -> s | None -> "none")
      (Some "touch ${current.archive.prefix}.done")
      (afoobar !t).archive_set_hooks.post_create_command;
    assert_equal ~printer:(function Some s -> s | None -> "none")
      (Some "rm ${current.archive.prefix}.done")
      (afoobar !t).archive_set_hooks.pre_clean_command;
    assert_equal
      ~printer:(fun s -> s)
      "foobar"
      (afoobar !t).base_prefix;
    assert_equal
      ~printer:(function Some d -> string_of_int d | None -> "<none>")
      (Some 2)
      (afoobar !t).max_incrementals;
    assert_equal ~printer:string_of_int
      3
      (List.assoc "foobar" !t.archive_sets).max_archives;

    (* 1st create *)
    create_ignore_result {!t with dry_run = true};
    create_ignore_result !t;
    assert_equal_dir_list
      ["foobar_20150926_full.1.dar";
       "foobar_20150926_full.done";

       "barbaz_20150926_full.1.dar"]
      (in_tmpdir ["srv"; "backup"]);

    (* 2nd create. *)
    create_ignore_result {(T.set_now_rfc3339 t "20150927") with dry_run = true};
    create_ignore_result !t;
    assert_equal_dir_list
      ["srv"; "etc"; "var"; "sync-done"]
      (in_tmpdir []);
    assert_equal_dir_list
      ["foobar_20150926_full.1.dar";
       "foobar_20150926_full.done";
       "foobar_20150926_incr00001.1.dar";
       "foobar_20150926_incr00001.done";

       "barbaz_20150926_full.1.dar";
       "barbaz_20150926_incr00001.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    assert_bigger_size
      (in_tmpdir ["srv"; "backup"; "foobar_20150926_full.1.dar"])
      (in_tmpdir ["srv"; "backup"; "foobar_20150926_incr00001.1.dar"]);

    (* Simulate a few days run. *)
    create_ignore_result (T.set_now_rfc3339 t "20150928");
    create_ignore_result (T.set_now_rfc3339 t "20150929");
    create_ignore_result (T.set_now_rfc3339 t "20150930");
    create_ignore_result (T.set_now_rfc3339 t "20151001");

    (* Check result of a few days *)
    assert_equal_dir_list
      ["srv"; "etc"; "var"; "sync-done"]
      (in_tmpdir []);
    assert_equal_dir_list create_current_files (in_tmpdir ["srv"; "backup"]);

    (* Check no changes with dry_run. *)
    clean {!t with dry_run = true};
    assert_equal_dir_list
      ["srv"; "etc"; "var"; "sync-done"]
      (in_tmpdir []);
    assert_equal_dir_list create_current_files (in_tmpdir ["srv"; "backup"]);

    (* 1st clean. *)
    clean !t;
    assert_equal_dir_list ["srv"; "etc"; "var"] (in_tmpdir []);
    assert_equal_dir_list clean_current_files (in_tmpdir ["srv"; "backup"]);

    (* 2nd clean. *)
    clean !t;
    assert_equal_dir_list clean_current_files (in_tmpdir ["srv"; "backup"])


let test_executable test_ctxt =
  let _, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let darckup cmd now_rfc3339 args =
    assert_command ~ctxt:test_ctxt (darckup_exec test_ctxt)
      ([cmd; "--verbose";
        "--ini"; in_tmpdir ["etc"; "darckup.ini"];
        "--no_ini_d"; "--now_rfc3339"; now_rfc3339] @ args)
  in
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"]
      (Examples.default
       ^ Examples.archive_set_foobar);
    (* Create etc/foobar.darrc. *)
    write_file ["etc"; "foobar.darrc"] Examples.darrc;
    darckup "create" "20150929" ["--all_archive_sets"];
    darckup "clean" "20150929" ["--all_archive_sets"];
    darckup "cronjob" "20150930" ["--all_archive_sets"];
    darckup "cronjob" "20151001" ["--all_archive_sets"];
    darckup "cronjob" "20151002" ["--all_archive_sets"];
    assert_equal_dir_list
      ["foobar_20150929_full.1.dar";
       "foobar_20151001_full.1.dar";
       "foobar_20151001_incr00001.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    ()

let test_catalog test_ctxt =
  let open FileUtil in
  let _, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let () =
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"]
      (Examples.default
       ^ Examples.archive_set_foobar
       ^ "create_catalog=true\n");
    (* Create etc/foobar.darrc. *)
    write_file ["etc"; "foobar.darrc"] Examples.darrc
  in
  let t = T.create test_ctxt in_tmpdir in
    create_ignore_result !t;
    (* Make sure that only the catalog will be used for incremental. *)
    rm [in_tmpdir ["srv"; "backup"; "foobar_20150926_full.1.dar"]];
    create_ignore_result (T.set_now_rfc3339 t "20150927");
    assert_equal_dir_list
      ["foobar_20150926_full_catalog.1.dar";
       "foobar_20150926_incr00001.1.dar";
       "foobar_20150926_incr00001_catalog.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    create_ignore_result (T.set_now_rfc3339 t "20150928");
    create_ignore_result (T.set_now_rfc3339 t "20150929");
    create_ignore_result (T.set_now_rfc3339 t "20150930");
    clean !t;
    assert_equal_dir_list
      ["foobar_20150928_full.1.dar";
       "foobar_20150928_full_catalog.1.dar";
       "foobar_20150928_incr00001.1.dar";
       "foobar_20150928_incr00001_catalog.1.dar";
       "foobar_20150930_full.1.dar";
       "foobar_20150930_full_catalog.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    ()

let test_catalog test_ctxt =
  let open FileUtil in
  let _, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let () =
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"]
      (Examples.default
       ^ Examples.archive_set_foobar
       ^ "create_catalog=true\n");
    (* Create etc/foobar.darrc. *)
    write_file ["etc"; "foobar.darrc"] Examples.darrc
  in
  let t = T.create test_ctxt in_tmpdir in
    create_ignore_result !t;
    (* Make sure that only the catalog will be used for incremental. *)
    rm [in_tmpdir ["srv"; "backup"; "foobar_20150926_full.1.dar"]];
    create_ignore_result (T.set_now_rfc3339 t "20150927");
    assert_equal_dir_list
      ["foobar_20150926_full_catalog.1.dar";
       "foobar_20150926_incr00001.1.dar";
       "foobar_20150926_incr00001_catalog.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    create_ignore_result (T.set_now_rfc3339 t "20150928");
    create_ignore_result (T.set_now_rfc3339 t "20150929");
    create_ignore_result (T.set_now_rfc3339 t "20150930");
    clean !t;
    assert_equal_dir_list
      ["foobar_20150928_full.1.dar";
       "foobar_20150928_full_catalog.1.dar";
       "foobar_20150928_incr00001.1.dar";
       "foobar_20150928_incr00001_catalog.1.dar";
       "foobar_20150930_full.1.dar";
       "foobar_20150930_full_catalog.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    ()

let test_always_incremental test_ctxt =
  let open FileUtil in
  let _, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let () =
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"]
      (Examples.default
       ^ Examples.archive_set_foobar
       ^ "always_incremental=true\n"
       ^ Examples.archive_set_barbaz
       ^ "darrc=barbaz.darrc\n"
       ^ "create_catalog=true\n");
    (* Create etc/foobar.darrc. *)
    write_file ["etc"; "foobar.darrc"] Examples.darrc;
    write_file ["etc"; "barbaz.darrc"] Examples.darrc
  in
  let t = T.create test_ctxt in_tmpdir in
  let has_error_log = ref false in
    assert_raises
      ArchiveSet.MissingFullArchive
      (fun () ->
         create_ignore_result
           {!t with
            log =
              (fun lvl s ->
                 if lvl = `Error then
                   has_error_log := true
                 else
                   !t.log lvl s)});
    assert_bool
      "At least one error emitted."
      !has_error_log;
    assert_equal_dir_list
      ["barbaz_20150926_full.1.dar";
       "barbaz_20150926_full_catalog.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    mv
      (in_tmpdir ["srv"; "backup"; "barbaz_20150926_full_catalog.1.dar"])
      (in_tmpdir ["srv"; "backup"; "foobar_20150926_full_catalog.1.dar"]);
    rm [in_tmpdir ["srv"; "backup"; "barbaz_20150926_full.1.dar"]];
    t := {!t with
              archive_sets =
                List.filter
                  (fun (aname, _) -> aname = "foobar") !t.archive_sets};
    create_ignore_result !t;
    assert_equal_dir_list
      ["foobar_20150926_full_catalog.1.dar";
       "foobar_20150926_incr00001.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    create_ignore_result !t; clean !t;
    create_ignore_result !t; clean !t;
    create_ignore_result !t; clean !t;
    create_ignore_result !t; clean !t;
    create_ignore_result !t; clean !t;
    assert_equal_dir_list
      ["foobar_20150926_full_catalog.1.dar";
       "foobar_20150926_incr00005.1.dar";
       "foobar_20150926_incr00006.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    ()


let test_always_incremental_allow_first test_ctxt =
  let _, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let () =
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"]
      (Examples.default
       ^ Examples.archive_set_foobar
       ^ "always_incremental=true\n"
       ^ "create_initial_archive=true\n");
    (* Create etc/foobar.darrc. *)
    write_file ["etc"; "foobar.darrc"] Examples.darrc;
  in
  let t = T.create test_ctxt in_tmpdir in
  create_ignore_result !t;
  assert_equal_dir_list
    ["foobar_20150926_full.1.dar"]
    (in_tmpdir ["srv"; "backup"]);
    ()


let test_encrypted test_ctxt =
  let _, in_tmpdir, write_file = setup_filesystem test_ctxt in
  let () =
    (* Create etc/darckup.ini. *)
    write_file ["etc"; "darckup.ini"]
      (Examples.default ^ Examples.archive_set_foobar);
    (* Create etc/foobar.darrc. *)
    write_file ["etc"; "foobar.darrc"]
      (Examples.darrc ^
       "create:
        -K 1234
        reference:
        -J 1234
       ");
  in
  let t = T.create test_ctxt in_tmpdir in
    create_ignore_result !t;
    create_ignore_result !t;
    assert_equal_dir_list
      ["foobar_20150926_full.1.dar";
       "foobar_20150926_incr00001.1.dar"]
      (in_tmpdir ["srv"; "backup"]);
    ()


let tests =
  [
    "ArchiveSet" >:: test_archive_set;
    "ArchiveCanIncludeIgnore" >:: test_archive_can_include_done;
    "ArchiveCanUseOnlyDone" >:: test_archive_can_use_only_done;
    "ArchiveOnlyCatalogsDoesntContainDone" >::
    test_archive_only_catalogs_doesnt_contain_done;
    "ArchiveOnlyVolumesDoesntContainDone" >::
    test_archive_only_volumes_doesnt_contain_done;
    "ArchiveSetTransition" >:: test_archive_set_transition;
    "load+clean+create" >:: test_load_clean_create;
    "Executable" >:: test_executable;
    "Catalog" >:: test_catalog;
    "AlwaysIncremental" >:: test_always_incremental;
    "AlwaysIncrementalAllowFirst" >:: test_always_incremental_allow_first;
    "Encrypted" >:: test_encrypted;
  ]


let () =
  run_test_tt_main
    ("Darckup" >::: tests)
