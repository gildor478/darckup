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

module TestArchive =
struct
end

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
             Printf.sprintf "unknown substitution variable %S" keyword)
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
      ~printer:string_of_size
      ~cmp:(fun sz1 sz2 -> size_compare sz1 sz2 > 0)
      (stat fn1).size (stat fn2).size

let tests = 
  [
    "ArchiveSet" >::
    (fun test_ctxt ->
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
              (List.map Archive.to_filenames
                 (snd (ArchiveSet.npop (ArchiveSet.length set) set))));

         (* next *)
         assert_equal
           ~msg:"ArchiveSet.next"
           ~printer:(fun s -> s)
           "foobar_20150907_full"
           (Archive.to_prefix (ArchiveSet.next set 1 "foobar_20150907"));
         assert_equal
           ~msg:"ArchiveSet.next"
           ~printer:(fun s -> s)
           "foobar_20150905_incr02"
           (Archive.to_prefix (ArchiveSet.next set 2 "foobar_20150907"));

         ()
    );

    "load+clean+create" >::
    (fun test_ctxt ->
       let t =
         {
           default with
               now_rfc3339 = "today";
               log = (fun lvl s -> logf test_ctxt lvl "%s" s);
         }
       in
       let tmpdir = bracket_tmpdir test_ctxt in
       let in_tmpdir lst = FilePath.make_filename (tmpdir :: lst) in
       let write_file lst cnt =
         write_file test_ctxt ["tmpdir", tmpdir] (in_tmpdir lst) cnt
       in
       let open FileUtil in
       let open FilePath in
       let () =
         (* Populate the filesystem. *)
         mkdir ~parent:true (in_tmpdir ["var"; "foobar"]);
         touch (in_tmpdir ["var"; "foobar"; "01.txt"]);
         write_file ["var"; "foobar"; "02.txt"] (String.make 1512 'x');
         mkdir ~parent:true (in_tmpdir ["var"; "barbaz"]);
         touch (in_tmpdir ["var"; "barbaz"; "01.txt"]);
         touch (in_tmpdir ["var"; "barbaz"; "02.txt"]);
         mkdir ~parent:true (in_tmpdir ["srv"; "backup"]);
         (* Create etc/darckup.ini. *)
         write_file ["etc"; "darckup.ini"]
           "[default]
            pre_create_command=true
            post_create_command=true
            ignore_glob_files=*.md5sums,*.meta

            [archive:foobar]
            backup_dir=${tmpdir}/srv/backup
            darrc=${tmpdir}/etc/foobar.darrc
            base_prefix=foobar
            max_incrementals=2
            max_archives=3

            [archive:barbaz]
            backup_dir=${tmpdir}/srv/backup
            darrc=${tmpdir}/etc/barbaz.darrc
            base_prefix=barbaz
            max_incrementals=1
            max_archives=3
           ";
         (* Create etc/foobar.darrc. *)
         write_file ["etc"; "foobar.darrc"]
           "create:
            -w
            -D
            -g var/foobar
            -R ${tmpdir}
            all:
            -Z *.Z
            -Z *.bz2
            -Z *.gz
            -Z *.jpg
            -Z *.mp3
            -Z *.mpg
            -Z *.tgz
            -Z *.zip
           ";
         (* Create etc/barbaz.darrc. *)
         write_file ["etc"; "barbaz.darrc"]
           "create:
            -w
            -D
            -g var/barbaz
            -R ${tmpdir}
            all:
            -Z *.Z
            -Z *.bz2
            -Z *.gz
            -Z *.jpg
            -Z *.mp3
            -Z *.mpg
            -Z *.tgz
            -Z *.zip
           "
       in
       let t = load t (in_tmpdir ["etc"; "darckup.ini"]) in
       let () =
         (* Check result of loading INI file. *)
         assert_equal ~printer:(function Some s -> s | None -> "none")
           (Some "true") t.pre_create_command;
         assert_equal ~printer:(function Some s -> s | None -> "none")
           (Some "true") t.post_create_command;
         StringListDiff.assert_equal
           ["*.md5sums"; "*.meta"] t.ignore_glob_files;
         StringListDiff.assert_equal
           ["foobar"; "barbaz"] (List.map fst t.archive_sets);
         assert_equal ~printer:(fun s -> s)
           "foobar" (List.assoc "foobar" t.archive_sets).base_prefix;
         assert_equal ~printer:string_of_int
           2 (List.assoc "foobar" t.archive_sets).max_incrementals;
         assert_equal ~printer:string_of_int
           3 (List.assoc "foobar" t.archive_sets).max_archives
      in
      let _ = create t in
      let () =
        (* Check result of first run. *)
        assert_equal_dir_list
          ["foobar_today_full.1.dar"; "barbaz_today_full.1.dar"]
          (in_tmpdir ["srv"; "backup"])
      in
      let t = {t with now_rfc3339 = "1dayafter"} in
      let _lst = create t in
      let () =
        (* Check result of second run. *)
        assert_command ~ctxt:test_ctxt
          "ls" ["-l"; "-a"; in_tmpdir ["srv"; "backup"]];
        assert_equal_dir_list
          ["foobar_today_full.1.dar"; "foobar_today_incr01.1.dar";
           "barbaz_today_full.1.dar"; "barbaz_today_incr01.1.dar"]
          (in_tmpdir ["srv"; "backup"]);
        assert_bigger_size
          (in_tmpdir ["srv"; "backup"; "foobar_today_full.1.dar"])
          (in_tmpdir ["srv"; "backup"; "foobar_today_incr01.1.dar"]);
      in
        ());
  ]

let () =
  run_test_tt_main
    ("Darckup" >::: tests)
