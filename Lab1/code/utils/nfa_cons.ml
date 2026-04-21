(**
  nfa_cons.ml --- Constructs an nfa from a reg
*)

open Alphabet
open Reg
open Automata

(* Section I : We construct automatas, following the inductive definition of Reg *)

(* The Epsilon Automaton *)
let nfa_cons_eps : mini_nfa =
  {
    nb_states = 1;
    delta = (fun (_:int) (_:nfa_letter) -> []);
    final = [0]
  }

(* The Any Automaton *)
let nfa_cons_any : mini_nfa =
  {
    nb_states = 2;
    delta = (fun (s:int) (_:nfa_letter) -> match s with |0 -> [1] |_ -> []);
    final = [1]
  }

(* The One Letter Automaton *)
let nfa_cons_letter (a:letter) : mini_nfa =
  {
    nb_states = 2;
    delta = (fun (s:int) (l:nfa_letter) -> if (s, l) = (0, Let a) then [1] else []);
    final = [1]
  }

(* The Or Automaton *)
let nfa_cons_or (n1:mini_nfa) (n2:mini_nfa) : mini_nfa=
  {
    nb_states = n1.nb_states + n2.nb_states + 1;
    delta = (fun (s:int) (l:nfa_letter) -> match (s, l) with
                                                          |(0, Eps) -> [1; n1.nb_states + 1]
                                                          |(0, _) -> []
                                                          |(_, _) when s <= n1.nb_states 
                                                              -> List.map ((+) 1) (n1.delta (s-1) l)
                                                          |(_, _) when s <= n1.nb_states + n2.nb_states
                                                              -> List.map ((+) (1 + n1.nb_states)) (n1.delta (s-1-n1.nb_states) l)
                                                          |_ -> []);
    final = (List.map ((+) 1) n1.final)@(List.map ((+) (1 + n1.nb_states)) n2.final)
  }

(* The Concat Automaton *)
let nfa_cons_concat (n1:mini_nfa) (n2:mini_nfa) : mini_nfa =
  {
    nb_states = n1.nb_states + n2.nb_states;
    delta = (fun (s:int) (l:nfa_letter) -> (if (List.mem s n1.final) && l = Eps then 
                                                n1.nb_states::(n1.delta s l)
                                            else if s < n1.nb_states then
                                                n1.delta s l
                                            else if s < n1.nb_states + n2.nb_states then
                                                List.map ((+) (n1.nb_states)) (n2.delta (s-n1.nb_states) l)
                                            else []));
    final = (List.map ((+) (n1.nb_states)) n2.final)
  }

(* The Kleene Star Automaton *)
let nfa_cons_star (n1:mini_nfa) : mini_nfa =
  {
    nb_states = n1.nb_states + 1;
    delta = (fun (s:int) (l:nfa_letter) -> (if (List.mem s n1.final) && l = Eps then 
                                                0::(n1.delta s l)
                                            else if s = 0 && l = Eps then 
                                                (n1.nb_states)::(n1.delta s l)
                                            else if s = n1.nb_states then []
                                            else n1.delta s l));
    final = [n1.nb_states]
  }

(* Section II : We can finally construct the final automaton ! *)
let rec nfa_cons (r : letter reg) : mini_nfa = 
  match r with
    |Eps            -> nfa_cons_eps
    |Any            -> nfa_cons_any
    |Lit(a)         -> nfa_cons_letter a
    |Or(r1, r2)     -> nfa_cons_or (nfa_cons r1) (nfa_cons r2)
    |Concat(r1, r2) -> nfa_cons_concat (nfa_cons r1) (nfa_cons r2)
    |Star(r')       -> nfa_cons_star (nfa_cons r')
