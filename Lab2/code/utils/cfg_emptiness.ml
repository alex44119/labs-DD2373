(*
  cfg_emptiness.ml --- Function that tests whether a given CFG is empty or not
*)

open Cfg

let one_step (c:cfg) (update_list : variable list) : variable list = 
  let is_alpha_gen (a : alpha) : bool = match a with
    |Terminal(_) -> true
    |Var(v) -> List.mem v update_list
  in
    (List.filter (fun x -> (List.exists (fun y -> List.for_all is_alpha_gen y) (c.productions x))) c.variables)

let rec iterate_states (c:cfg) (update_list : variable list) : variable list =
  let new_update = (one_step c update_list) in 
    if (List.equal (=) update_list new_update) then 
      new_update
    else
      iterate_states c new_update

let is_generating (c:cfg) (v:variable) = List.mem v (iterate_states c [])

let is_empty (c:cfg) = is_generating c Start