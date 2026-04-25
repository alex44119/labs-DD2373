(**
  simulation.ml --- Simulates the automata with an input string
*)

open Alphabet
open Automata
open Basics

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

(* NFA simulation *)
let rec delta_hat_nfa (n : nfa) (s : state) (input : letter list) : state list = match input with
  |[] -> [s]
  |h::t -> dedup (List.flatten (List.map (fun x -> delta_hat_nfa n x t) (n.delta s (Let h))))


let rec delta_hat_nfa_early_stop (n : nfa) (s : state) (input : letter list) : state list =
  (* Returns the first time he reaches a final state *)
if List.mem s n.final then [s] else
match input with
  |[] -> [s]
  |h::t -> dedup (List.flatten (List.map (fun x -> delta_hat_nfa_early_stop n x t) (n.delta s (Let h))))

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