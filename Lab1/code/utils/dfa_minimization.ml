(**
  nfa_dfa.ml --- Constructs the minimized DFA using the Table Filling Algorithm

  (Section I) 
    tab_fil_alg : dfa -> string array array

  (Section II) 
    dfa_temp_dfa_min : dfa -> temporary_dfa
*)

open Alphabet
open Automata
open Basics

(* Section I : The Table Filling Algorithm *)

(* 1. This function tests a given letter 'a' for a given pair of states (i, j) *)
let tab_update_letter (d : dfa) (tab : string array array) (i : state) (j : state) (a : letter) = 
  (* Updates the table tab if needed upon reading letter a on states i and j, according to the Table Filling algorithm *)

  let res = tab.(d.delta i a).(d.delta j a) in 
  if (res != "") then 
    tab.(i).(j) <- (String.make 1 a)^res

(* 2. This function iterates previous function on all letters and all pair of states *)
let tab_update (d : dfa) (tab : string array array) = 
  (* Updates the table tab according to the Table Filling algorithm *)

  let n = Array.length tab in 

  for i=0 to n-1 do 
    for j=0 to n-1 do 
      if (tab.(i).(j) == "") then 
        List.iter (tab_update_letter d tab i j) d.alphabet
    done
  done

(* 3. All final states should be epsilons (eps) in the table.
PS : Empty strings are already used to say that no distinguishing element has been found. *)
let tab_init (d : dfa) : string array array = 
  (* Initializes the table with empty strings, according to the Table Filling algorithm *)

  let n = d.max_state in 
  let res = Array.make_matrix n n "" in

  for i=0 to n-1 do 
    for j=0 to n-1 do 
      if (List.mem i d.final != List.mem j d.final) then res.(i).(j) <- eps;
    done
  done;
  res

(* 4. And finally : The Table Filling Algorithm *)
let tab_fil_alg (d : dfa) : string array array = 
  (* Uses the Table Filling algorithm to create a table of equivalence tab *)

  let tab = tab_init d in
  let tab_former = ref [||] in 

  while not (array_equal (array_equal String.equal) tab !tab_former) do 
    tab_former := Array.map Array.copy tab;
    tab_update d tab
  done;
  
  tab

(* Section II : Construction of the minimum DFA *)

(* 1. Return all the equivalence classes (= states of the minimum DFA) *)
let equivalence_states (tab : string array array) : temporary_state list = 
  (* Returns the list of all the equivalence classes *)

  let n = Array.length tab in
  let check = Array.make n false in 

  let rec aux i j l =
    match () with
    | _ when i = n -> []
    | _ when j = 0 && check.(i) -> aux (i + 1) 0 []
    | _ when j = 0 && not check.(i) ->
        check.(i) <- true;
        aux i 1 [i]
    | _ when j = n -> l :: aux (i + 1) 0 []
    | _ when check.(j) -> aux i (j + 1) l
    | _ when tab.(i).(j) = "" ->
        check.(j) <- true;
        aux i (j + 1) (j :: l)
    | _ -> aux i (j + 1) l
  in aux 0 0 []

(* 2. State mapping functions in both directions *)
let maps (eq : state list list) (s : state) : temporary_state = 
  (* Returns the list of equivalent states that corresponds to the state s *)
  List.nth eq s;;

let maps_rev (eq : state list list) (t_state : temporary_state) : state = 
  (* Returns the number of the equivalence class to which the state t belongs *)
  match t_state with 
   |[] -> failwith "State t is empty, this is impossible for equivalent classes."
   |t::_ ->
      let rec aux (l : state list list) (s : state) = 
        match l with 
        | [] -> failwith "State t is too big, it does not exist"
        | hd::_ when List.mem t hd -> s
        | _::tl -> aux tl (s + 1)
      in aux eq 0;;

(* 3. Construct the temporary dfa (states are still to be renamed) *)
let dfa_temp_dfa (d : dfa) : temporary_dfa = 
  (* Returns the DFA constructed from the NFA n. *)

  let eq = equivalence_states (tab_fil_alg d) in
  
  {
    states = eq;
    alphabet = d.alphabet;
    delta = (fun x a -> maps eq (d.delta (maps_rev eq x) a));
    initial = maps eq d.initial;
    final = dedup (List.map (maps eq) d.final)
  }