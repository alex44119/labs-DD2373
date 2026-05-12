(*
  Main code for the project.
*)

open Utils.Server
open Utils.Cfg_emptiness
open Utils.Dfa
open Utils.Dfa_parser
open Utils.Cfg_prod
open Utils.Flowgraph_parser;;

launch_server 8000 300;;
print_string "The server is running:\n\nhttp://localhost:8000/viewer\n\n";;

let filename_spec = Sys.argv.(1);;
let dfa = build_dfa filename_spec;;
let comp_dfa = dfa_comp dfa;;

let filename_cfg = Sys.argv.(2);;
let fg = build_fg filename_cfg;;

let cfg = cfg_prod comp_dfa fg;;

let res = is_empty cfg;;

match res with
| None -> print_endline "No counter-example found ! Flowgraph accepted ! "
| Some(l) -> 
      print_endline "Flowgraph rejected with the following counter-example :";
      List.iter print_string (List.map ((^) "; ") l);;








