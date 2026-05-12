(*
  Performance testing.
*)

(*
  Performance testing for the project
*)

open Unix

open Utils.Cfg_emptiness
open Utils.Dfa
open Utils.Dfa_parser
open Utils.Cfg_prod
open Utils.Flowgraph_parser

(* Function that runs the pipeline and measures execution time *)
let run_test spec_file cfg_file =
  let start_time = gettimeofday () in

  (* Same pipeline as main.ml *)
  let dfa = build_dfa spec_file in
  let fg = build_fg cfg_file in
  let comp_dfa = dfa_comp dfa in
  let cfg = cfg_prod comp_dfa fg in
  let _ = find_counter_example cfg in

  let end_time = gettimeofday () in
  let duration = end_time -. start_time in

  Printf.printf "Test (%s, %s) took: %.6f seconds\n"
    spec_file cfg_file duration;
  flush Stdlib.stdout


(* List of test cases *)
let tests = [
  ("testcases/Vote/Vote_v.spec", "testcases/Vote/Vote_ne.cfg");
  ("testcases/Vote/Vote_gv.spec", "testcases/Vote/Vote_ne.cfg");
  ("testcases/Simple/simple.spec", "testcases/Simple/simple.cfg");
  ("testcases/EvenOdd/EvenOdd1b.spec", "testcases/EvenOdd/EvenOdd.cfg");
  ("testcases/EvenOdd/EvenOdd1a.spec", "testcases/EvenOdd/EvenOdd.cfg");
]

let () =
  List.iter (fun (spec, cfg) -> run_test spec cfg) tests

