(*
  dfa.ml --- Defines automata types
*)

type state = int

(* For the DFA, the edges are labeled by methods (the word method is already taken by something else in Ocaml) *)
type meth = string

(* Definition of Deterministic Finite Automata*)
type dfa = 
  {
    nb_states : state; 
    alphabet : meth list;
    delta : meth list array array; (* Entries of this matrix are state * state *)
    initial : state;
    final : state list
  }