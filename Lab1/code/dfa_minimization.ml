type state = int;;

type dfa = 
  {
    states : state list;
    alphabet : string list;
    delta : state -> string -> state;
    initial : state;
    final : state list
  }

(* je remets les types pour de la clarté mais ça doit supprimer à terme *)

let tab_update_letter (d : dfa) (tab : string array array) (i : state) (j : state) (a : string) = 
  (* Updates the table tab if needed upon reading letter a on states i and j, according to the Table Filling algorithm *)

  let res = tab.(d.delta i a).(d.delta j a) in 
  if (res != "") then 
    tab.(i).(j) <- a ^ res

let tab_update (d : dfa) (tab : string array array) = 
  (* Updates the table tab according to the Table Filling algorithm *)

  let n = Array.length tab in 

  for i=0 to n-1 do 
    for j=0 to n-1 do 
      if (tab.(i).(j) == "") then 
        List.iter (tab_update_letter d tab i j) d.alphabet
    done
  done;;

let tab_init (d : dfa) : string array array = 
  (* Initializes the table with empty strings, according to the Table Filling algorithm *)
  (* HERE WE SUPPOSE THE STATES ARE 0,...,N-1 !!!!! TO BE CHECKED *)
  
  let n = List.length d.states in 
  let res = Array.make_matrix n n "" in

  for i=0 to n-1 do 
    for j=0 to n-1 do 
      if (List.mem i d.final != List.mem j d.final) then res.(i).(j) <- epsilon;
    done
  done;
  res;;

let array_equal eq a b =
  (* Useful function to check if two arrays a and b are equal according to the comparative function eq (Array.equal) *)

  let n = Array.length a in
  n = Array.length b &&
  let rec aux i =
    if i = n then true
    else if eq a.(i) b.(i) then aux (i + 1)
    else false
  in aux 0;;

let tab_fil_alg (d : dfa) : string array array = 
  (* Uses the Table Filling algorithm to create a table of equivalence tab *)

  let tab = tab_init d in
  let tab_former = ref [||] in 

  while not (array_equal (array_equal String.equal) tab !tab_former) do 
    tab_former := Array.map Array.copy tab;
    tab_update d tab
  done;
  
  tab;;

let equivalence_states (tab : string array array) : state list list = 
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
  in aux 0 0 [];;


(* The following two functions are mapping functions that are useful to the "renaming" step *)

let maps (s : state) (eq : state list list) : state list = 
  (* Returns the list of equivalent states that corresponds to the state s *)

  List.nth eq s;;

let maps_rev (t : state) (eq : state list list) : state = 
  (* Returns the number of the equivalence class to which the state t belongs *)

  let rec aux (l : state list list) (s : state) = 
    match l with 
    | [] -> failwith "State t is too big, it does not exist"
    | hd::tl when List.mem t hd -> s
    | hd::tl -> aux tl (s + 1)
  in aux eq 0;;

let delta_min (d: dfa) (eq : state list list) (s : state) (str : string) : state = 
  (* Will allow us to create the delta function for the minimized DFA *)

  let t = maps s eq in let hd = List.hd t in
  maps_rev (d.delta hd str) eq;;

let initial_min (d : dfa) (eq : state list list) : state = 
  (* Returns the initial state of the minimized DFA *)

  maps_rev d.initial eq;;

let final_min (d : dfa) (eq : state list list) : state list = 
  (* Returns the states of the minimized DFA, as a list *)
  
  let rec aux (l : state list list) = 
    match l with 
    | [] -> []
    | hd::tl when List.mem (List.hd hd) d.final -> (maps_rev (List.hd hd) eq) :: (aux tl) 
    | hd::tl -> aux tl
  in aux eq;;

let dfa_min (d : dfa) : dfa = 
  (* Returns the minimized dfa *)

  let tab = tab_fil_alg d in 
  let eq = equivalence_states tab in 
  let f (k : int) = k in 

  {
    states = List.init (List.length eq) f;
    alphabet = d.alphabet;
    delta = delta_min d eq;
    initial = initial_min d eq;
    final = final_min d eq
  };;




