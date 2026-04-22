(**
  automata.ml --- Defines automata types
*)

open Alphabet

(* Definition of the type state, and its infix comparators *)
type state = int

(* Definition of the temporary state *)
type temporary_state = state list

(* Definition of Deterministic Finite Automata*)
type dfa = 
  {
    nb_states : state; 
    alphabet : letter list;
    delta : state -> letter -> state;
    initial : state;
    final : state list
  }

(* Definition of temporary DFAs*)
type temporary_dfa = 
  {
    states : temporary_state list;
    alphabet : letter list;
    delta : temporary_state -> letter -> temporary_state;
    initial : temporary_state;
    final : temporary_state list
  }

(* Definition of Non-deterministic Finite Automata*)
type nfa = 
  {
    nb_states : state; 
    alphabet : nfa_letter list;
    delta : state -> nfa_letter -> state list;
    initial : state;
    final : state list
  }

type mini_nfa = (* Initial state should be always 0 and alphabet is any char *)
  {
    nb_states : state; 
    delta : state -> nfa_letter -> state list;
    final : state list
  }