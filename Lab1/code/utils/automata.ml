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

(* Definition of the temporary state, flatten and infix comparators *)
type temporary_state = state list
let flatten (l : temporary_state list) : temporary_state = 
  (* Returns the list of states in the lists in l, without repetition. *)
  let rec aux (to_check : state list list) (seen : state list) = 
    match to_check with 
    | [] -> seen
    | hd::tl -> match hd with 
      | [] -> aux tl seen
      | x::y ->
          if List.mem x seen then
            aux (y::tl) seen
          else
            aux (y::tl) (x::seen)
  in List.sort cmp (aux l []);;

(* Definition of Deterministic Finite Automata*)
type dfa = 
  {
    max_state : state;
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
    max_state : state;
    alphabet : nfa_letter list;
    delta : state -> nfa_letter -> state list;
    initial : state;
    final : state list
  }