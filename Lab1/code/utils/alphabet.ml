(** 
    alphabet.ml --- Defines the alphabet and the special character ε
*)

(* Alphabet *)
type letter = char
type nfa_letter = Eps | Let of letter

(* Special character *)
let eps = "ε" (* This is a unicode string *)
