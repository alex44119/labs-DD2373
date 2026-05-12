(**
    viewer.ml --- Generates the JSON code to visualize DFA and Flowgraph objects and CFG printers
*)

open Dfa
open Flowgraph
open Cfg

type json = string

(* ---------- Helpers ---------- *)

let write (filename : string) (content : string) : unit =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let list_to_json_array (f : 'a -> string) (l : 'a list) : json =
  "[" ^ String.concat ", " (List.map f l) ^ "]"

let json_string (s : string) : json =
  Printf.sprintf "%S" s

let bool_to_json (b : bool) : json =
  if b then "true" else "false"

let meth_to_json (m : meth) : json =
  json_string m

let state_to_json (s : state) : json =
  string_of_int s

let node_to_json (n : node) : json =
  Printf.sprintf
    {|{
      "id": %d,
      "entry": %s,
      "return": %s,
      "meth": %s
    }|}
    n.id
    (bool_to_json n.entry)
    (bool_to_json n.return)
    (meth_to_json n.meth)

(* Generic transition merger.

   Input transitions are of the shape:

     from_state, to_state, labels

   where labels is a meth list.

   It merges transitions with the same source and target.
*)
let merge_transitions transitions =
  let table = Hashtbl.create 16 in

  List.iter
    (fun (from_s, to_s, labels) ->
      let key = (from_s, to_s) in
      let existing =
        match Hashtbl.find_opt table key with
        | Some l -> l
        | None -> []
      in
      Hashtbl.replace table key (labels @ existing))
    transitions;

  Hashtbl.fold
    (fun (from_s, to_s) labels acc ->
      let label_str =
        labels
        |> List.sort_uniq String.compare
        |> String.concat ", "
      in
      (from_s, to_s, label_str) :: acc)
    table
    []

(* ---------- DFA ---------- *)

let dfa_viewer (ds : dfa list) : json =
  let one_dfa (d : dfa) =
    let states = List.init d.nb_states (fun i -> i) in

    (* Collect raw transitions from the matrix.

       d.delta.(i).(j) is the list of methods labeling
       the edge from state i to state j.
    *)
    let raw_transitions =
      List.flatten
        (List.map
           (fun from_s ->
             List.filter_map
               (fun to_s ->
                 let labels = d.delta.(from_s).(to_s) in
                 match labels with
                 | [] -> None
                 | _ -> Some (from_s, to_s, labels))
               states)
           states)
    in

    let transitions =
      merge_transitions raw_transitions
      |> List.map
           (fun (from_s, to_s, label) ->
             Printf.sprintf
               {|{ "from": %d, "to": %d, "label": %s }|}
               from_s
               to_s
               (json_string label))
    in

    Printf.sprintf
      {|{
        "type": "dfa",
        "states": %s,
        "alphabet": %s,
        "transitions": [%s],
        "initial": %d,
        "final": %s
      }|}
      (list_to_json_array state_to_json states)
      (list_to_json_array meth_to_json d.alphabet)
      (String.concat ", " transitions)
      d.initial
      (list_to_json_array state_to_json d.final)
  in

  list_to_json_array one_dfa ds

(* ---------- Flowgraph ---------- *)

let flowgraph_viewer (fgs : flowgraph list) : json =
  let one_flowgraph (fg : flowgraph) =

    let nodes = fg.nodes in
    let node_ids = List.map (fun n -> n.id) nodes in

    (* Collect raw transitions from the matrix.

       fg.edge_func.(i).(j) is the list of methods labeling
       the edge from node i to node j.
    *)
    let raw_transitions =
      List.flatten
        (List.map
           (fun from_s ->
             List.filter_map
               (fun to_s ->
                 let labels = fg.edge_func.(from_s).(to_s) in
                 match labels with
                 | [] -> None
                 | _ -> Some (from_s, to_s, labels))
               node_ids)
           node_ids)
    in

    let transitions =
      merge_transitions raw_transitions
      |> List.map
           (fun (from_s, to_s, label) ->
             Printf.sprintf
               {|{ "from": %d, "to": %d, "label": %s }|}
               from_s
               to_s
               (json_string label))
    in

    Printf.sprintf
      {|{
        "type": "flowgraph",
        "methods": %s,
        "nodes": %s,
        "transitions": [%s]
      }|}
      (list_to_json_array meth_to_json fg.methods)
      (list_to_json_array node_to_json nodes)
      (String.concat ", " transitions)
  in

  list_to_json_array one_flowgraph fgs

(* ---------- Store ---------- *)

let store_dfa (d : dfa) : unit =
  write "viewer/dfa.json" (dfa_viewer [d])

let store_dfas (ds : dfa list) : unit =
  write "viewer/dfa.json" (dfa_viewer ds)

let store_flowgraph (fg : flowgraph) : unit =
  write "viewer/flowgraph.json" (flowgraph_viewer [fg])

let store_flowgraphs (fgs : flowgraph list) : unit =
  write "viewer/flowgraph.json" (flowgraph_viewer fgs)


(* ---------- CFG Pretty-printing helpers ---------- *)

let string_of_x_var (x : x_var) : string =
  match x with
  | Node n -> string_of_int n.id
  | Terminal m -> m

let string_of_variable (v : variable) : string =
  match v with
  | Start -> "Start"
  | Tuple (p, x, q) ->
      Printf.sprintf "(%d, %s, %d)"
        p
        (string_of_x_var x)
        q

let string_of_alpha (a : alpha) : string =
  match a with
  | Var v -> string_of_variable v
  | Terminal m -> m

let string_of_sentential_form (sf : sentential_form) : string =
  match sf with
  | [] -> "ε"
  | _ ->
      sf
      |> List.map string_of_alpha
      |> String.concat " "

let string_of_production (lhs : variable) (rhs_list : sentential_form list) : string =
  let lhs_str = string_of_variable lhs in
  let rhs_str =
    match rhs_list with
    | [] -> "∅"
    | _ ->
        rhs_list
        |> List.map string_of_sentential_form
        |> String.concat " | "
  in
  lhs_str ^ " -> " ^ rhs_str

(* ---------- Main printer ---------- *)

let print_cfg (g : cfg) : unit =
  List.iter
    (fun v ->
      let rhs_list = g.productions v in
      print_endline (string_of_production v rhs_list))
    g.variables

(* ---------- CFG printer with renamed variables: A, B, C, ... ---------- *)

let fresh_variable_name (i : int) : string =
  let base =
    String.make 1 (Char.chr (Char.code 'A' + (i mod 26)))
  in
  let suffix = i / 26 in
  if suffix = 0 then base
  else base ^ string_of_int suffix

let renamed_variable_table (g : cfg) : (variable, string) Hashtbl.t =
  let table = Hashtbl.create 32 in

  (* Start is always Start *)
  Hashtbl.add table Start "Start";

  let counter = ref 0 in

  List.iter
    (fun v ->
      match v with
      | Start -> ()
      | _ ->
          if not (Hashtbl.mem table v) then begin
            Hashtbl.add table v (fresh_variable_name !counter);
            incr counter
          end)
    g.variables;

  table

let string_of_renamed_variable
    (table : (variable, string) Hashtbl.t)
    (v : variable)
  : string =
  match Hashtbl.find_opt table v with
  | Some name -> name
  | None -> failwith "Unknown variable while printing CFG"

let string_of_renamed_alpha
    (table : (variable, string) Hashtbl.t)
    (a : alpha)
  : string =
  match a with
  | Var v -> string_of_renamed_variable table v
  | Terminal m -> m

let string_of_renamed_sentential_form
    (table : (variable, string) Hashtbl.t)
    (sf : sentential_form)
  : string =
  match sf with
  | [] -> "ε"
  | _ ->
      sf
      |> List.map (string_of_renamed_alpha table)
      |> String.concat " "

let string_of_renamed_production
    (table : (variable, string) Hashtbl.t)
    (lhs : variable)
    (rhs_list : sentential_form list)
  : string =
  let lhs_str = string_of_renamed_variable table lhs in
  let rhs_str =
    match rhs_list with
    | [] -> "∅"
    | _ ->
        rhs_list
        |> List.map (string_of_renamed_sentential_form table)
        |> String.concat " | "
  in
  lhs_str ^ " -> " ^ rhs_str


(* Final CFG printer *)
let print_renamed_cfg (g : cfg) : unit =
  let table = renamed_variable_table g in

  List.iter
    (fun v ->
      let rhs_list = g.productions v in
      print_endline (string_of_renamed_production table v rhs_list))
    g.variables