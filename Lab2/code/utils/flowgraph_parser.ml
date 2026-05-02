(*
  flowgraph_parser.ml --- Parses the .cfg file to create the associated Flow Graph. 
*)

open Flowgraph
open Dfa

type line_fg = Node of string * meth * meth | Edge of string * string * meth;;

let parse_line (line : string) : line_fg = 
  let line = String.trim line in 

  let parts = String.split_on_char ' ' line in

  match parts with 
  | nature :: name :: met :: tl when String.equal nature "node" -> 
          let met = String.sub met 5 (String.length met - 6) in 
          if (List.is_empty tl) then 
            (Node(name, met, ""))
          else
            (Node(name, met, List.hd tl))
  | nature :: src :: goal :: met :: [] when String.equal nature "edge" -> Edge(src, goal, met)
  | _ -> failwith "Error when parsing one of the lines";;



let first_read (lines : string list) : string list * node list * meth list = 
  let rec aux (lines : string list) (name_l : string list) (nodes : node list) (met_l : meth list) = 
    match lines with
    | [] -> name_l, nodes, met_l
    | hd :: tl -> 
          let line = parse_line hd in 
          match line with 
          | Edge(_) -> aux tl name_l nodes met_l
          | Node(name, met, str) when String.equal str "" -> 
                let node = 
                  {
                    id = List.length name_l;
                    entry = false;
                    return = false;
                    meth = met
                  } in 

                if (List.mem met met_l) then 
                  (aux tl (name :: name_l) (node :: nodes) met_l)
                else 
                  (aux tl (name :: name_l) (node :: nodes) (met :: met_l))
          | Node(_) -> failwith "Error when reading the last word of a line Node"
  in aux lines [] [] [];;



let second_read (lines : string list) (delt : meth list array array) (name_l : string list) : unit = 
  let rec aux (lines : string list) : unit = 
    match lines with 
    | [] -> ()
    | hd :: tl -> 
          let line = parse_line hd in
          match line with 
          | Node(_) -> ()
          | Edge(src, goal, met) -> 
                let a = List.find_index (String.equal src) name_l in 
                let b = List.find_index (String.equal goal) name_l in 
                match (a,b) with 
                | Some(i), Some(j) -> delt.(i).(j) <- met :: delt.(i).(j); aux tl
                | _,_ -> failwith "Error when renaming the methods to states"
  in aux lines;;



(* Returns all the lines of the file which path is filename *)
let read_lines filename =
  let ic = open_in filename in
  let rec aux acc =
    try aux (input_line ic :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in aux [];;



(* Returns the Flow Graph written on the input file *)
let build_fg filename : flowgraph =
  let lines = read_lines filename in

  let name_l, nodes, met_l = first_read lines in 
  let n = List.length nodes in
  let delt = Array.make_matrix n n [] in 

  second_read lines delt name_l;
 
  {
    methods = met_l;
    nodes = nodes;
    edge_func = delt;
  };;

