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


(* Useful functions for testing *)
let print_node (n : node) : unit = 
  print_endline "Node :";
  print_int n.id;
  print_endline "Entry :";
  print_endline (string_of_bool n.entry);
  print_endline "Return :";
  print_endline (string_of_bool n.return);
  print_endline "Method :";
  print_endline n.meth;;

let print_fg (fg : flowgraph) : unit = 
  print_endline "Flowgraph : ";
  print_endline "Methods : ";
  List.iter print_endline fg.methods;
  print_endline "Nodes :";
  List.iter print_node fg.nodes;
  print_endline "Edge function test :";
  List.iter print_endline fg.edge_func.(3).(4);