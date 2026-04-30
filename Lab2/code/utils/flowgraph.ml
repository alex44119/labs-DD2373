(*
  flowgraphs.ml --- Defines flowgraphs types
*)

open Dfa

type node = int

type flowgraph = 
  {
    methods : meth list;
    nb_nodes : int;
    node_meth : node -> meth;
    entry_nodes : node list;
    ret_nodes : node list;
    edge_func : node -> meth -> node
  }

(* Returns the entry node of the main function *)
let get_entry_node (fg : flowgraph) (m : meth) : node = 
  let rec aux l = 
    match l with
    | []  -> failwith "Error : this method is not detected"
    | hd::_ when fg.node_meth hd = m -> hd
    | _::tl -> aux tl
  in aux fg.entry_nodes;;

