
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

let tests = 
  [
    "Simple" >::
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
           6
           (ArchiveSet.length set);

         (* last *)
         StringListDiff.assert_equal
           ["foobar_20150905_full.1.dar"]
           (Archive.to_filenames (ArchiveSet.last set));
         ()
    );
  ]

let () =
  run_test_tt_main
    ("Darckup" >::: tests)
