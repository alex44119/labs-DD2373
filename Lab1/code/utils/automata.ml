(**
  automata.ml --- Defines automata types

  type "state" and type "dfa"

  type "temporary_state" and type "temporary_dfa"

  type "nfa"
*)

open Alphabet

(* Definition of the type state, and its infix comparators *)
type state = int
let cmp (a : state) (b : state) : int = 
  match a,b with
  | _ when a = b -> 0
  | _ when a < b -> -1
  | _ -> 1
let (<)  s1 s2 = cmp s1 s2 < 0
let (>)  s1 s2 = cmp s1 s2 > 0
let (<=) s1 s2 = cmp s1 s2 <= 0
let (>=) s1 s2 = cmp s1 s2 >= 0

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