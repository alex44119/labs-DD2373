(**
  Main code for the project.
*)

open Utils.Parser
open Utils.Nfa_cons
open Utils.Nfa_dfa
open Utils.Basics
open Utils.Automata
open Utils.Alphabet
open Utils.Dfa_minimization
open Utils.Simulation

let lines = read_all_lines ();;

let alphabet_unparsed, regexp_unparsed, text_lines = match lines with
  | a :: r :: inputs -> a, r, inputs
  | _ -> failwith "Error: input must contain at least 2 lines (alphabet + regex)";;

Printf.printf "Alphabet: %s\nRegex: %s\n" alphabet_unparsed regexp_unparsed;;

let alphabet = string_to_char_list alphabet_unparsed;;
let regexp = let r = parser (".*(" ^ regexp_unparsed ^ ")") in
      match r with
      |Value(a) -> a
      |Error(e) -> failwith e;;
            
let nfa = mini_to_nfa (nfa_cons regexp) (Eps::(List.map (fun x -> Let x) alphabet));;
let temp_dfa = nfa_temp_dfa nfa;;
let dfa = temp_dfa_dfa temp_dfa;;
let min_dfa = temp_dfa_dfa (dfa_temp_dfa dfa);;

List.iter (fun line ->
  let input = string_to_char_list line in 
  if List.mem (delta_hat_dfa_early_stop min_dfa min_dfa.initial input) min_dfa.final then
    print_endline line
) text_lines;;