type state = int;;

type dfa = 
  {
    states : state list;
    alphabet : string list;
    delta : state -> string -> state;
    initial : state;
    final : state list
  }

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
  Array.make n (Array.make n "")

let array_equal eq a b =
  let n = Array.length a in
  n = Array.length b &&
  let rec aux i =
    if i = n then true
    else if eq a.(i) b.(i) then aux (i + 1)
    else false
  in aux 0;;

let tab_fil_alg (d : dfa) : string array array = 
  (* Uses the Table Filling algorithm to create a table tab of equivalence *)

  let tab = tab_init d in
  let tab_former = ref [||] in 

  while not (array_equal (array_equal String.equal) tab !tab_former) do 
    tab_former := Array.map Array.copy tab;
    tab_update d tab
  done;
  
  tab;;

let equivalence_array (tab : string array array) : state list list = 
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

let maps (s : state) (eq : state list list) : state list = 
  List.nth eq s;;

let maps_rev (t : state) (eq : state list list) : state = 
  let rec aux (l : state list list) (s : state) = 
    match l with 
    | [] -> failwith "State t is too big, it does not exist"
    | hd::tl when List.mem t hd -> s
    | hd::tl -> aux tl (s + 1)
  in aux eq 0;;



