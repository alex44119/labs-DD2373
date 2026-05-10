(*
  dfa.ml --- Defines automata types
*)

type state = int

(* For the DFA, the edges are labeled by methods (the word method is already taken by something else in Ocaml) *)
type meth = string

(* Definition of Deterministic Finite Automata*)
type dfa = 
  {
    nb_states : int; 
    alphabet : meth list;
    delta : meth list array array; (* Entries of this matrix are state * state *)
    initial : state;
    final : state list
  }

(* DFA Complement *)
let final_states_comp (final : state list) (nb_states : int) : state list =
  List.filter (fun x -> not (List.mem x final)) (List.init nb_states (fun x->x))

let dfa_comp (d : dfa) : dfa = 
  {
    nb_states = d.nb_states; 
    alphabet = d.alphabet; 
    delta = d.delta; 
    initial = d.initial; 
    final = final_states_comp d.final d.nb_states
  }

(* Useful function for testing *)
let print_dfa (d : dfa) : unit = 
  print_int d.nb_states;
  print_endline "";
  List.iter print_string d.alphabet;
  print_endline "";
  List.iter print_string d.delta.(2).(2);
  print_endline "";
  print_int d.initial;
  print_endline "";
  List.iter print_int d.final;; 