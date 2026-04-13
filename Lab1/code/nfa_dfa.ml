let epsilon = "ε" ;;

type state = int;; 
type temporary_state = state list;;

type dfa = 
  {
    states : state list;
    alphabet : string list;
    delta : state -> string -> state;
    initial : state;
    final : state list
  }

  type temporary_dfa = 
  {
    states : temporary_state list;
    alphabet : string list;
    delta : temporary_state -> string -> temporary_state;
    initial : temporary_state;
    final : temporary_state list
  }
  
type nfa = 
  {
    states : state list;
    alphabet : string list;
    delta : string -> state -> state list;
    initial : state;
    final : state list 
  }
  
  

(* UTILITIES *)

let cmp (a : state) (b : state) : int = 
  (* Compares states to be able to sort the list of states *)
  
  match a,b with
  | _ when a = b -> 0
  | _ when a < b -> -1
  | _ -> 1;;


  let flatten (l : state list list) : state list = 
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


let rec eclose (n : nfa) (q : state) : temporary_state = 
  (* Returns the epsilon closure of state q in the NFA N, in increasing order. *)
  
  let rec aux (to_visit : state list) (visited : state list) =
    match to_visit with
    | [] -> visited
    | x :: xs ->
        if List.mem x visited then
          aux xs visited
        else
          let eps_moves = n.delta epsilon x in
          aux (eps_moves @ xs) (x :: visited)
  in List.sort cmp (aux [q] []);; 

  
let rec is_final (n : nfa) (q : temporary_state) : bool = 
  (* Returns true iff in the temporary state q  there is a final state of the NFA n. *)
  
  match q with 
  | [] -> false
  | x::y when List.mem x n.final -> true
  | x::y -> is_final n y;;





(* NFA TO TEMPORARY DFA *)

let nfa_temp_dfa_initial (n : nfa) : temporary_state = 
  (* Returns the starting state (as a temporary state) of the DFA constructed from the NFA n. 
     The new starting state is the epsilon closure of the old one. *)
  
  eclose n n.initial;;


let nfa_temp_dfa_delta (n : nfa) (q : temporary_state) (s : string) : temporary_state = 
  (* Returns the state (as a temporary state) that we would end on by following the delta function of the DFA constructed from the NFA n. *)
  
  match q with 
  | [] -> []
  | _ -> let q_s = flatten (List.map (n.delta s) q) in 
      let q_eclose = List.map (eclose n) q_s in 
      flatten q_eclose;;


let nfa_temp_dfa_states (n : nfa) : temporary_state list = 
  (* Returns the list of states (each state is a temporary state) of the DFA constructed from the NFA n. *)
  let alph = n.alphabet in 

  let rec aux (to_check : temporary_state list) (seen : temporary_state list) : temporary_state list = 
    match to_check with
    | [] -> seen
    | hd::tl when List.mem hd seen -> aux tl seen
    | hd::tl -> aux ((List.map (nfa_temp_dfa_delta n hd) alph) @ to_check) (hd::seen)
  in aux [nfa_temp_dfa_initial n] [];;


let nfa_temp_dfa_final (n : nfa) : temporary_state list = 
  (* Returns the list of final states (each final state is a temporary state) of the DFA constructed from the NFA n. *)
  
  List.filter (is_final n) (nfa_temp_dfa_states n)


let nfa_temp_dfa (n : nfa) : temporary_dfa = 
  (* Returns the DFA constructed from the NFA n. *)
  
  {
    states = nfa_temp_dfa_states n;
    alphabet = n.alphabet;
    delta = nfa_temp_dfa_delta n;
    initial = nfa_temp_dfa_initial n;
    final = nfa_temp_dfa_final n
  }
               




(* TEMPORARY DFA TO DFA AFTER RENAMING *)

let maps_dfa (s : state) (states : temporary_state list) : temporary_state = 
  (* Returns the temporary state corresponding to state s *)

  List.nth states s;;


let maps_rev_dfa (t : temporary_state) (states : temporary_state list) : state option = 
  (* Returns the state corresponding to the temporary state t *)

  let f (q : temporary_state) : bool = List.equal (=) q t in
  List.find_index f states;;


let temp_dfa_dfa_delta (d : temporary_dfa) (q : state) (s : string) : state = 
  (* Will allow us to create the delta function for the renamed DFA *)

  let t = maps_dfa q d.states in let res = d.delta t s in 
  match maps_rev_dfa res d.states with
  | Some i -> i
  | None -> failwith "Problem when using maps_rev_dfa in temp_dfa_dfa_delta";;


let temp_dfa_dfa_initial (d : temporary_dfa) : state = 
  (* Returns the starting state of the renamed DFA. *)

  match maps_rev_dfa d.initial d.states with 
  | Some i -> i
  | None -> failwith "Problem when using maps_rev_dfa in temp_dfa_dfa_initial";;


let temp_dfa_dfa_final (d : temporary_dfa) : state list = 
  (* Returns the final states of the renamed DFA *)

  let rec aux (l : temporary_state list) = 
    match l with 
    | [] -> []
    | hd::tl -> 
      match maps_rev_dfa hd d.states with 
      | Some i -> i :: (aux tl)
      | None -> failwith "Problem when using maps_rev_dfa in temp_dfa_dfa_final"
    in aux d.final;;


let temp_dfa_dfa (d : temporary_dfa) : dfa = 
  (* Returns the renamed DFA from the temporary DFA d *)

  let f (k : int) = k in 

  {
    states = List.init (List.length d.states) f;
    alphabet = d.alphabet;
    delta = temp_dfa_dfa_delta d ;
    initial = temp_dfa_dfa_initial d ;
    final = temp_dfa_dfa_final d
  };;