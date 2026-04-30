(*
  cfg.ml --- Defines Context Free Grammar types and some functions
*)

open Dfa
open Flowgraph

type x_var = Node of node | Method of meth

type variable = Start | Tuple of state * x_var * state

type alpha = Var of variable | Method of meth

type sentential_form = alpha list

type cfg = 
  {
    variables : variable list;
    terminals : meth list;
    productions : variable -> sentential_form list
  }

