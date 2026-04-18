(**
  nfa_dfa.ml --- Converts NFA to DFA by the subset construction

  (Section  I) 
    nfa_temp_dfa : nfa -> temporary_dfa

  (Section II) 
    temp_dfa_dfa : temporary_dfa -> dfa
*)

open Alphabet
open Automata
open Basics

(* Section I : From NFA to temporary DFA *)

(* 1. NFA states to temporary DFA states *)
let eclose (n : nfa) (q : state) : temporary_state = 
  (* Returns the epsilon closure of state q in the NFA N, in increasing order. *)

  let rec aux (to_visit : state list) (visited : state list) =
    match to_visit with
    | [] -> visited
    | x :: xs ->
        if List.mem x visited then
          aux xs visited (* Ingored if already visited *)
        else
          let eps_moves = n.delta x Eps in
            aux (eps_moves @ xs) (x :: visited) (* BFS algorithm *)
  in List.sort cmp (aux [q] [])

let rec is_final (n : nfa) (q : temporary_state) : bool = 
  (* Returns true iff there is a final state of the NFA n in the temporary state q. *)
  match q with 
  | []   -> false
  | x::y -> List.mem x n.final || is_final n y

(* 2. NFA init state to temporary DFA init states *)
let nfa_temp_dfa_initial (n : nfa) : temporary_state = 
  (* The starting state is just the epsilon eclosure of n.initial ! *)
  eclose n n.initial

(* 3. NFA transition function to temporary DFA transition function *)
let nfa_temp_dfa_delta (n : nfa) (q : temporary_state) (s : letter) : temporary_state = 
  (* Returns the state (as a temporary state) that we would end on by following the delta function of the DFA constructed from the NFA n. *)
  
  match q with 
  | [] -> []
  | _ -> let q_s = dedup (List.flatten (List.map (fun x -> n.delta x (Let s)) q)) in 
      let q_eclose = List.map (eclose n) q_s in 
      dedup (List.flatten q_eclose)


(* 4. NFA alphabet to temporary DFA alphabet *)
let rec nfa_temp_dfa_alphabet (nfa_alphabet : nfa_letter list) : letter list =
  match nfa_alphabet with
    |[] -> []
    |Eps::t -> nfa_temp_dfa_alphabet t
    |Let(a)::t -> a::(nfa_temp_dfa_alphabet t)

(* 5. Select only the accessible temporary DFA states *)
let nfa_temp_dfa_states (n : nfa) : temporary_state list = 
  (* Returns the list of states (each state is a temporary state) of the DFA constructed from the NFA n. *)
  let alph = n.alphabet in 

  let rec aux (to_check : temporary_state list) (seen : temporary_state list) : temporary_state list = 
    match to_check with
    | [] -> seen
    | hd::tl when List.mem hd seen -> aux tl seen
    | hd::_ -> aux ((List.map (nfa_temp_dfa_delta n hd) (nfa_temp_dfa_alphabet alph)) @ to_check) (hd::seen)
  in aux [nfa_temp_dfa_initial n] []

(* 6. NFA final states to temporary DFA final states *)
let nfa_temp_dfa_final (n : nfa) : temporary_state list = 
  (* Returns the list of final states (each final state is a temporary state) of the DFA constructed from the NFA n. *)
  
  List.filter (is_final n) (nfa_temp_dfa_states n)


(* 7. Final mapping from NFA to temporary DFA *)
let nfa_temp_dfa (n : nfa) : temporary_dfa = 
  (* Returns the DFA constructed from the NFA n. *)
  
  {
    states = nfa_temp_dfa_states n;
    alphabet = nfa_temp_dfa_alphabet n.alphabet;
    delta = nfa_temp_dfa_delta n;
    initial = nfa_temp_dfa_initial n;
    final = nfa_temp_dfa_final n
  }

(* Section II : From temporary DFA to DFA *)

(* 1. States mapping in both directions *)
let maps_dfa (states : temporary_state list) (s : state) : temporary_state = 
  (* Returns the temporary state corresponding to state s *)
  List.nth states s


let maps_rev_dfa (states : temporary_state list) (t : temporary_state) : state option = 
  (* Returns the state corresponding to the temporary state t *)

  let f (q : temporary_state) : bool = List.equal (=) q t in
  List.find_index f states


(* 2. Transition function mapping *)
let temp_dfa_dfa_delta (d : temporary_dfa) (q : state) (s : letter) : state = 
  (* Will allow us to create the delta function for the renamed DFA *)

  let t = maps_dfa d.states q in let res = d.delta t s in 
  match maps_rev_dfa d.states res with
  | Some i -> i
  | None -> failwith "Problem when using maps_rev_dfa in temp_dfa_dfa_delta"

(* 3. Initial states mapping *)
let temp_dfa_dfa_initial (d : temporary_dfa) : state = 
  (* Returns the starting state of the renamed DFA. *)

  match maps_rev_dfa d.states d.initial with 
  | Some i -> i
  | None -> failwith "Problem when using maps_rev_dfa in temp_dfa_dfa_initial"

(* 4. Final states mapping *)
let temp_dfa_dfa_final (d : temporary_dfa) : state list = 
  (* Returns the final states of the renamed DFA *)

  let rec aux (l : temporary_state list) = 
    match l with 
    | [] -> []
    | hd::tl -> 
      match maps_rev_dfa d.states hd with 
      | Some i -> i :: (aux tl)
      | None -> failwith "Problem when using maps_rev_dfa in temp_dfa_dfa_final"
    in aux d.final

(* 5. Final mapping from temporary DFA to DFA *)
let temp_dfa_dfa (d : temporary_dfa) : dfa = 
  (* Returns the renamed DFA from the temporary DFA d *)

  {
    max_state = List.length d.states;
    alphabet = d.alphabet;
    delta = temp_dfa_dfa_delta d ;
    initial = temp_dfa_dfa_initial d ;
    final = temp_dfa_dfa_final d
  }