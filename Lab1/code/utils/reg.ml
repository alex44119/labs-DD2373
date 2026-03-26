(** 
    Inductive definition of Reg type
*)

type 'a reg =
  | Eps                         (* Epsilon *)
  | Any                         (* Any literal : represented by a dot "." *)
  | Lit of 'a                   (* A specific literal of the alphabet *)
  | Or of 'a reg * 'a reg       (* Union *)
  | Concat of 'a reg * 'a reg   (* Concatenation *)
  | Star of 'a reg              (* Kleene Star *)

(* In the parser, "R+" will be changed to "RR*",
   and "R?" will be changed to "(R|Eps)" *)

(** Specialized version using ASCII characters *)
type char_reg = char reg