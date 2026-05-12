(*
  Main code for the project.
*)

open Utils.Server
open Utils.Cfg_emptiness
open Utils.Dfa
open Utils.Dfa_parser
open Utils.Cfg_prod
open Utils.Flowgraph_parser
open Utils.Viewer

(* Name of the files containing the dfa and the flow graph *)
let filename_spec = Sys.argv.(1);;
let filename_cfg = Sys.argv.(2);;

(* Parse dfa and flow graph *)
let dfa = build_dfa filename_spec;;
let fg = build_fg filename_cfg;;

(* Construct the complementary dfa *)
let comp_dfa = dfa_comp dfa;;

(* Build the product CFG *)
let cfg = cfg_prod comp_dfa fg;;

(* Test if the CFG is empty *)

(* Print and show *)
store_dfa dfa;;
store_flowgraph fg;;
print_renamed_cfg cfg;;

let counter_example = find_counter_example cfg;;
match counter_example with
  |None -> print_endline "No counter-example found ! Flowgraph accepted !" 
  |Some ce -> print_endline ("Flowgraph rejected with the following counter-example : "^ce);;

launch_server 8000 300;;
print_string "The server is running:\n\nhttp://localhost:8000/viewer\n\n";;



