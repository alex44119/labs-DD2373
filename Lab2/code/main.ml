(*
  Main code for the project.
*)

(* open Utils.Server *)
open Utils.Dfa
open Utils.Dfa_parser
open Utils.Cfg_prod
open Utils.Cfg
open Utils.Flowgraph
open Utils.Flowgraph_parser;;

(*launch_server 8000 300;;
print_string "The server is running:\n\nhttp://localhost:8000/viewer\n\n";;*)

let filename_spec = Sys.argv.(1);;
let dfa = build_dfa filename_spec;;
let comp_dfa = dfa_comp dfa;;

let filename_cfg = Sys.argv.(2);;
let fg = build_fg filename_cfg;;

let cfg = cfg_prod comp_dfa fg;;

print_dfa comp_dfa;;
print_fg fg;;
print_cfg cfg;;






