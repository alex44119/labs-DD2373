(*
  cfg_emptiness.ml --- Function that tests whether a given CFG is empty or not
*)

open Cfg

let one_step (c:cfg) (update_list : variable list) 
  : variable list * (variable, sentential_form) Hashtbl.t = 
    let is_alpha_gen (a : alpha) : bool = match a with
      |Terminal(_) -> true
      |Var(v) -> List.mem v update_list
    in
      let d = Hashtbl.create 10 in 
        (List.filter 
          (fun x ->
            (is_alpha_gen (Var x)) ||
            (List.exists (fun y -> (Hashtbl.add d x y) ; List.for_all is_alpha_gen y) (c.productions x))
          ) 
        c.variables), d

let rec iterate_states (c:cfg) (update_list : variable list) 
  : variable list * (variable, sentential_form) Hashtbl.t =
    let new_update, d = (one_step c update_list) in 
      if (List.equal (=) update_list new_update) then 
        new_update, d
      else
        iterate_states c new_update

let rec back_path (dico : (variable, sentential_form) Hashtbl.t) (a : alpha) : (string list) =
  match a with 
    |Terminal t -> [t]
    |Var v -> let l = Hashtbl.find dico v in 
                List.flatten (List.map (fun a -> back_path dico a) l)

let is_generating (c:cfg) (v:variable) : (string list) option = 
  let update_list, dico = iterate_states c [] in 
    if List.mem v update_list then 
      Some (back_path dico (Var v))
    else 
      None

let is_empty (c:cfg) : string list option = is_generating c Start