(*
  cfg.ml --- Defines Context Free Grammar types and some functions
*)

open Dfa
open Flowgraph

type x_var = Node of node | Terminal of meth

type variable = Start | Tuple of state * x_var * state

type alpha = Var of variable | Terminal of meth

type sentential_form = alpha list

type cfg = 
  {
    variables : variable list;
    terminals : meth list;
    productions : variable -> sentential_form list
  }



  (* Useful functions for testing *)

let print_variable (v : variable) : unit = 
  match v with 
  | Start -> print_endline "Start"
  | Tuple(qa, x, qb) ->
        match x with
        | Node(n) -> print_int qa; print_string ", "; print_int n.id; print_string ", "; print_int qb; print_endline ""
        | Terminal(m) -> print_int qa; print_string ", "; print_string m; print_string ", "; print_int qb; print_endline "";;

let rec print_sent (s : sentential_form) : unit = 
  match s with 
  | [] -> print_endline ""
  | hd :: tl -> match hd with
                | Var (v) -> print_variable v; print_sent tl
                | Terminal(meth) -> print_endline meth; print_sent tl;;


let print_cfg (g : cfg) : unit = 
  print_endline "Variables :";
  List.iter print_variable g.variables;
  print_endline "Terminals : ";
  List.iter print_endline g.terminals;
  print_endline "Productions :";
  let n = {id = 0; entry = true; return = false; meth = "main"} in 
  List.iter print_sent (g.productions (Tuple(0,Node(n),0)));;
  