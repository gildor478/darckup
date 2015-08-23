
open OUnit2
open Darckup

let tests = 
  [
    "Archive.next" >::
    (fun test_ctxt ->
       ());
  ]

let () =
  run_test_tt_main
    ("Darckup" >::: tests)
