let epsilon = "ε" ;;

type state = Nb of int | Set of state list;; 

type dfa = 
  {
    states : state list;
    alphabet : string list;
    delta : state -> string -> state;
    initial : state;
    final : state list
  }
  
type nfa = 
  {
    states : state list;
    alphabet : string list;
    delta : string -> state -> state list;
    initial : state;
    final : state list 
  }
  
  



let cmp (a : state) (b : state) : int = 
  (* Compares states to be able to sort the list of states *)
  
  match a,b with
  | Nb(x),Nb(y) when x = y -> 0
  | Nb(x),Nb(y) when x < y -> -1
  | Nb(x),Nb(y) when x > y -> 1
  | _,_ -> failwith "Error in cmp : thoses states are not int. ";;



let rec eclose (n : nfa) (q : state) : state list = 
  (* We look for the epsilon closure in the NFA N, thus q is an int. 
     Returns the epsilon closure of q in N, in increasing order. *)
  
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



let nfa_dfa_states (n : nfa) : state list = 
  (* Returns the list of states (each state is a list of states) of the DFA constructed from the NFA n. *)
  let rec aux (to_check : state list) (seen : state list) : state list = 
    match to_check with 
    | [] -> seen
    |hd::tl -> let close = Set(eclose n hd) in 
        if List.mem close seen then 
          aux tl seen
        else 
          aux tl (close :: seen)
  in aux n.states [];;
  
  
  
let nfa_dfa_initial (n : nfa) : state = 
  (* Returns the starting state (as a list of state) of the DFA constructed from the NFA n. 
     The new starting state is the epsilon closure of the old one. *)
  
  Set(eclose n n.initial);;



let rec is_final (n : nfa) (q : state) : bool = 
  (* Returns true iff in the state q (represented as a list of states) there is a final state of the NFA n. *)
  
  match q with 
  | Set([]) -> false
  | Set(x::y) when List.mem x n.final -> true
  | Set(x::y) -> is_final n (Set(y))
  | _ -> failwith "Error in is_final : the state is not a list of states. ";;



let nfa_dfa_final (n : nfa) : state list = 
  (* Returns the list of final states (each final state is a list of states) of the DFA constructed from the NFA n. *)
  
  List.filter (is_final n) (nfa_dfa_states n)
  
  

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



let nfa_dfa_delta (n : nfa) (q : state) (s : string) : state = 
  (* Returns the state (as a list of state) that we would end on by following the delta function of the DFA constructed from the NFA n. *)
  
  match q with 
  | Set(l) -> let l_s = flatten (List.map (n.delta s) l) in 
      let l_eclose = List.map (eclose n) l_s in 
      Set(flatten l_eclose)
  | Nb(x) -> failwith "Error in nfa_dfa_delta : the state is not a list of states. ";;



let nfa_dfa (n : nfa) : dfa = 
  (* Returns the DFA constructed from the NFA n. *)
  
  {
    states = nfa_dfa_states n;
    alphabet = n.alphabet;
    delta = nfa_dfa_delta n;
    initial = nfa_dfa_initial n;
    final = nfa_dfa_final n
  }
               
  