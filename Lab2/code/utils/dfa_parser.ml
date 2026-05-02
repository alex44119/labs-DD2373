(*
  dfa_parser.ml --- Parses the .spec file to create the associated DFA. 
*)

open Dfa

type line_dfa = 
  {
    init : bool;
    src : string;
    src_fin : bool;
    met : meth;
    goal : string;
    goal_fin : bool
  };;

let parse_line (line : string) : line_dfa = 
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
          (true, String.sub bef 1 (String.length bef - 2))
        else 
          (false, String.sub bef 1 (String.length bef - 2))
      in let src = bef in 

      let (goal_fin, aft) =
        if aft.[1] = '(' then
          (true, String.sub aft 2 (String.length aft - 3))
        else 
          (false, String.sub aft 2 (String.length aft - 3))
      in let goal = aft in 
      
      {
        init = init; 
        src = src;
        src_fin = src_fin;
        met = met;
        goal = goal;
        goal_fin = goal_fin 
      }

  | _ -> failwith ("Invalid line: " ^ line)




let first_read (lines : string list) : (meth list * meth list * meth * meth list) = 
  let rec aux (line_l : string list) (state_l : meth list) (meth_l : meth list) (init_l : meth list) (fin_l : meth list) = 
    match line_l with 
    | [] when List.length init_l != 1 -> failwith "Error with initial state"
    | [] -> (state_l, meth_l, List.hd init_l, fin_l)
    | hd::tl -> 
          let line = parse_line hd in 

          let res_state = 
            if (not (List.mem line.src state_l)) then 
              (if (not (List.mem line.goal state_l)) then 
                (line.src :: line.goal :: state_l)
              else
                (line.src :: state_l))
            else
              (if (not (List.mem line.goal state_l)) then 
                (line.goal :: state_l)
              else
                (state_l))
          in

          let res_fin = 
            if (line.src_fin && not (List.mem line.src fin_l)) then 
              (if (line.goal_fin && not (List.mem line.goal fin_l)) then 
                (line.src :: line.goal :: fin_l)
              else
                (line.src :: fin_l))
            else
              (if (line.goal_fin && not (List.mem line.goal fin_l)) then 
                (line.goal :: fin_l)
              else
                (fin_l))
          in

          let res_init = 
            if (line.init && not (List.mem line.src init_l)) then 
              (line.src :: init_l)
            else
              (init_l)
          in 

          let res_meth = 
            if (not (List.mem line.met meth_l)) then 
              (line.met :: meth_l)
            else
              (meth_l)
          in 

          aux tl res_state res_meth res_init res_fin
  in aux lines [] [] [] [];;



let second_read (lines : string list) (delt : meth list array array) (state_l : meth list) : unit = 
  let rec aux (lines : string list) : unit = 
    match lines with 
    | [] -> ()
    | hd :: tl -> 
          let line = parse_line hd in
          let a = List.find_index (String.equal line.src) state_l in 
          let b = List.find_index (String.equal line.goal) state_l in 
          match (a,b) with 
          | Some(i), Some(j) -> delt.(i).(j) <- line.met :: delt.(i).(j); aux tl
          | _,_ -> failwith "Error when renaming the methods to states"
  in aux lines;;



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
  let n = List.length state_l in 
  let delt = Array.make_matrix n n [] in 

  second_read lines delt state_l;

  let a = 
    match List.find_index (String.equal init) state_l with
    | Some(i) -> i 
    | None -> failwith "Error when renaming the methods to states"
  in 

  let rec aux fin_l = 
    match fin_l with 
    | [] -> []
    | hd :: tl -> 
      let a = 
        match List.find_index (String.equal hd) state_l with
          | Some(i) -> i 
          | None -> failwith "Error when renaming the methods to states"
      in a :: (aux tl)
    in 
  
  {
    nb_states = n;
    alphabet = meth_l;
    delta = delt; 
    initial = a;
    final = aux fin
  };;

