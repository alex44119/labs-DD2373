(**
  This is the example from the homework 2.
*)

open Utils.Parser
open Utils.Nfa_cons
open Utils.Nfa_dfa
open Utils.Basics
open Utils.Automata
open Utils.Alphabet
open Utils.Viewer
open Utils.Dfa_minimization

let regexp_unparsed : string = "ε|ab(ab)*"
let alphabet_unparsed : string = "abc";;
let regexp = let r = parser regexp_unparsed in
  match r with
    |Value(a) -> a
    |Error(e) -> failwith e
let alphabet = string_to_char_list alphabet_unparsed;;

let nfa = mini_to_nfa (nfa_cons regexp) (Eps::(List.map (fun x -> Let x) alphabet))
let temp_dfa = nfa_temp_dfa nfa
let dfa = temp_dfa_dfa temp_dfa;;
let min_dfa = temp_dfa_dfa (dfa_temp_dfa dfa);;

write "nfa.json" (nfa_viewer [nfa]);;
write "temp_dfa.json" (temporary_dfa_viewer [temp_dfa]);;
write "dfa.json" (dfa_viewer [dfa; min_dfa]);;

launch_server 8000 300;;
print_string "The server is running:\n\nhttp://localhost:8000/viewer\n\n";;