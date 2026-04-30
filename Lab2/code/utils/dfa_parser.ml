(*
  dfa_parser.ml --- Parses the .spec file to create the associated DFA. 
*)

open Dfa

let extract_state s =
  let s = String.trim s in
  let len = String.length s in
  let core =
    if s.[0] = '(' || s.[0] = '[' then
      String.sub s 1 (len - 2)
    else s
  in
  int_of_string (String.sub core 1 (String.length core - 1))

let is_final s =
  let s = String.trim s in
  s.[0] = '('



let parse_line (line : string) : bool * string * bool * meth * string * bool = 
  let line = String.trim line in 
  
  let (init, line) =
    if String.length line >= 2 && String.sub line 0 2 = "=>" then
      (true, String.sub line 2 (String.length line - 2))
    else
      (false, line)
  in

  let parts = String.split_on_char '-' line in
  match parts with
  | [bef; label; aft] ->
      let met = label in 

      let (src_fin, bef) =
        if bef.[0] = '(' then
          (true, String.sub bef 1 (String.length bef - 1))
        else 
          (false, String.sub bef 1 (String.length bef - 1))
      in let src = bef in 

      let (goal_fin, aft) =
        if aft.[1] = '(' then
          (true, String.sub aft 2 (String.length bef - 2))
        else 
          (false, String.sub aft 2 (String.length bef - 2))
      in let goal = aft in 

      init, src, src_fin, met, goal, goal_fin

  | _ -> failwith ("Invalid line: " ^ line)




let first_read (lines : string list) : (meth list * meth list * meth * meth list) = 
  let rec aux line_l state_l meth_l init_l fin_l = 
    match line_l with 
    | [] when List.length init_l != 1 -> failwith "Error with initial state"
    | [] -> (state_l, meth_l, List.hd init_l, fin_l)
    | hd::tl -> 
          let init, src, src_fin, met, goal, goal_fin = parse_line hd in 
          let res_state = ref state_l in 
          let res_fin = ref fin_l in 
          let res_init = ref init_l in 
          let res_meth = ref meth_l in 

          if init && not (List.mem src !res_init) then (res_init := src :: !res_init);

          if not (List.mem src !res_state) then (res_state :=  src :: !res_state);
          if not (List.mem goal !res_state) then (res_state :=  goal :: !res_state);

          if src_fin && not (List.mem src !res_fin) then (res_fin :=  src :: !res_fin);
          if goal_fin && not (List.mem goal !res_fin) then (res_fin :=  goal :: !res_fin);

          if not (List.mem met !res_meth) then (res_meth :=  met :: !res_meth);

          aux tl !res_state !res_meth !res_init !res_fin
  in aux lines [] [] [] [];;


              
(* Returns all the lines of the file which path is filename *)
let read_lines filename =
  let ic = open_in filename in
  let rec aux acc =
    try aux (input_line ic :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in aux [];;



(* Returns the DFA written on the input file *)
let build_dfa filename : dfa =
  let lines = read_lines filename in

  let state_l, meth_l, init, fin = first_read lines in 
  (* renommer les états à un moment *)
  let n = List.length state_l in 
  let delt = Array.make_matrix n n [] in 

  second_read lines delt;

  {
    nb_states = n;
    alphabet = meth_l;
    delta = delt; 
    initial = init;
    final = fin
  }

