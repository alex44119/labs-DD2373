(*
  cfg_cons.ml --- Functions creating the CFG from a FG and a specification DFA
*)

open Cfg
open Dfa
open Flowgraph


let prod_cfg (d : dfa) (fg : flowgraph) (v : variable) : sentential_form list = 
  match v with
  | Start -> 
        let v0 = get_entry_node fg "main" in 
        let q0 = d.initial in 
        List.map (fun x -> [Var(Tuple(q0, Node(v0), x))]) (d.final)
  | Tuple(qa, vi, qb) ->
        let res = ref [] in 




