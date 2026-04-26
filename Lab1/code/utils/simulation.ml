(**
  simulation.ml --- Simulates the automata with an input string
*)

open Alphabet
open Automata
open Nfa_dfa

(* DFA simulation *)
let rec delta_hat_dfa (d : dfa) (s : state) (input : letter list) : state = 
match input with
  |[] -> s
  |h::t -> delta_hat_dfa d (d.delta s h) t


let rec delta_hat_dfa_early_stop (d : dfa) (s : state) (input : letter list) : state = 
  (* Returns the first time he reaches a final state *)
if List.mem s d.final then s else
match input with
  |[] -> s
  |h::t -> delta_hat_dfa_early_stop d (d.delta s h) t

(* Temporary DFA simulation *)
let rec delta_hat_tempdfa (d : temporary_dfa) (s : temporary_state) (input : letter list) : temporary_state = 
match input with
  |[] -> s
  |h::t -> delta_hat_tempdfa d (d.delta s h) t


let rec delta_hat_tempdfa_early_stop (d : temporary_dfa) (s : temporary_state) (input : letter list) : temporary_state = 
  (* Returns the first time he reaches a final state *)
if List.mem s d.final then s else
match input with
  |[] -> s
  |h::t -> delta_hat_tempdfa_early_stop d (d.delta s h) t

(* Usefull simulation function *)
let simulate (a : automaton) (input : letter list) : bool = match a with
  |DFA(d) -> List.mem (delta_hat_dfa_early_stop d d.initial input) d.final
  |TempDFA(d) -> List.mem (delta_hat_tempdfa_early_stop d d.initial input) d.final
  |NFA(n) -> let d = nfa_temp_dfa n in List.mem (delta_hat_tempdfa_early_stop d d.initial input) d.final
  |MinNFA(mini_n, alphabet) -> let n = (mini_to_nfa mini_n alphabet) in 
             let d = nfa_temp_dfa n in List.mem (delta_hat_tempdfa_early_stop d d.initial input) d.final