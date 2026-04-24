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


let read_all_lines () =
  let rec aux acc =
    try
      let line = read_line () in
      aux (line :: acc)
    with End_of_file -> List.rev acc
  in
  aux [];;


let simulation (min_d : dfa) (line : string) : bool = 
  let n = String.length line in 
  let res = ref false in 
  let i = ref 0 in
  let state = ref min_d.initial in

  while not !res && !i < n do 
    state := min_d.delta !state line.[!i];
    incr i;
    if List.mem (!state) min_d.final then res := true
  done;

  !res;;


let () =
  let lines = read_all_lines () in
  match lines with
  | alphabet :: regex :: text_lines ->
    begin
      Printf.printf "Alphabet: %s\nRegex: %s\n" alphabet regex; 
      let alph_list = string_to_char_list alphabet in
      let regexp = let r = parser (".*(" ^ regex ^ ")") in
        match r with
        |Value(a) -> a
        |Error(e) -> failwith e
      in
              
      let nfa = mini_to_nfa (nfa_cons regexp) (Eps::(List.map (fun x -> Let x) alph_list)) in 
      let temp_dfa = nfa_temp_dfa nfa in 
      let dfa = temp_dfa_dfa temp_dfa in 
      let min_dfa = temp_dfa_dfa (dfa_temp_dfa dfa) in 
      

      List.iter (fun line ->
        if simulation min_dfa line then
          print_endline line
      ) text_lines
    end

  | _ -> failwith "Error: input must contain at least 2 lines (alphabet + regex)"
;;

