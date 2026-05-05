(*
  flowgraphs.ml --- Defines flowgraphs types
*)

open Dfa

type node = 
  {
    id : int;
    entry : bool;
    return : bool;
    meth : meth
  }

type flowgraph = 
  {
    methods : meth list;
    nodes : node list; (* The id of each node is its place in this list *)
    edge_func : meth list array array
  }

(* Returns the entry node of the main function *)
let get_entry_node (fg : flowgraph) (m : meth) : node = 
  let rec aux l = 
    match l with
    | []  -> failwith "Error : this method is not detected"
    | hd::_ when (hd.entry && String.equal hd.meth m) -> hd
    | _::tl -> aux tl
  in aux fg.nodes;;

