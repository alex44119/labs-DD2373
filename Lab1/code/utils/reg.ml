(** 
    reg.ml --- Inductive definition of Reg type
*)

type 'a reg =
  | Eps                         (* Epsilon *)
  | Any                         (* Any literal : represented by a dot "." *)
  | Lit of 'a                   (* A specific literal of the alphabet *)
  | Or of 'a reg * 'a reg       (* Union *)
  | Concat of 'a reg * 'a reg   (* Concatenation *)
  | Star of 'a reg              (* Kleene Star *)

(* Notice that "r+" = "rr*" and "r?" = "(r|Eps)" *)

type letter = char