(*
  cfg_emptiness.ml --- Function that tests whether a given CFG is empty or not
*)

open Cfg
open Dfa

type proof = meth list

let one_step (c:cfg) (update_list : variable list) (dico : (alpha, proof) Hashtbl.t) = 
  let is_alpha_gen (a : alpha) : bool = match a with
    |Terminal(_) -> true
    |Var(v) -> List.mem v update_list
  in
    List.filter (fun x -> (is_alpha_gen (Var x)) ||
      match List.find_opt (fun y -> List.for_all is_alpha_gen y) (c.productions x) with
      | Some y ->
          let values =
            List.map (fun i -> Hashtbl.find dico i) y
            |> List.flatten
          in
          Hashtbl.replace dico (Var x) values;
          true
      | None ->
          false
    ) c.variables


let rec iterate_states (c:cfg) (update_list : variable list) (dico : (alpha, proof) Hashtbl.t) : variable list =
  let new_update = (one_step c update_list dico) in 
    if (List.equal (=) update_list new_update) then 
      new_update
    else
      iterate_states c new_update dico

let init_dico (c:cfg) : (alpha, proof) Hashtbl.t =
  let dico = Hashtbl.create (List.length c.terminals) in
  List.iter (fun t ->
    Hashtbl.add dico (Terminal t) [t]
  ) c.terminals;
  dico


let is_generating (c:cfg) (v:variable) : string option = let d = init_dico c in
  if List.mem v (iterate_states c [] d) then 
    Some (String.concat ", " (Hashtbl.find d (Var v)))
  else 
    None

let find_counter_example (c:cfg) = is_generating c Start