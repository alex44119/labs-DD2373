(**
    viewer.ml --- Generates the JSON code to visualize the automata
*)

open Alphabet
open Automata
open Basics

type json = string

(* ---------- Helpers ---------- *)

let letter_to_json = function
  | Eps -> "\"ε\""
  | Let c -> "\"" ^ String.make 1 c ^ "\""

let char_to_string c = String.make 1 c

(* Generic transition merger *)
let merge_transitions add_label transitions =
  let table = Hashtbl.create 16 in

  List.iter (fun (from_s, to_s, label) ->
    let key = (from_s, to_s) in
    let existing =
      match Hashtbl.find_opt table key with
      | Some l -> l
      | None -> []
    in
    Hashtbl.replace table key (label :: existing)
  ) transitions;

  Hashtbl.fold (fun (from_s, to_s) labels acc ->
    let label_str =
      labels
      |> List.map add_label
      |> List.sort_uniq String.compare
      |> String.concat ", "
    in
    (from_s, to_s, label_str) :: acc
  ) table []

(* ---------- DFA ---------- *)

let dfa_viewer (ds : dfa list) : json =
  let one_dfa (d : dfa) =
    let states = List.init d.nb_states (fun i -> i) in

    (* Collect raw transitions *)
    let raw_transitions =
      List.flatten (
        List.map (fun state ->
          List.map (fun letter ->
            let next_state = d.delta state letter in
            (state, next_state, letter)
          ) d.alphabet
        ) states
      )
    in

    (* Merge *)
    let transitions =
      merge_transitions char_to_string raw_transitions
      |> List.map (fun (from_s, to_s, label) ->
          Printf.sprintf
            "{ \"from\": %d, \"to\": %d, \"label\": \"%s\" }"
            from_s to_s label)
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
      (list_to_json_array string_of_int states)
      (list_to_json_array (fun c -> "\"" ^ char_to_string c ^ "\"") d.alphabet)
      (String.concat ", " transitions)
      d.initial
      (list_to_json_array string_of_int d.final)
  in
  list_to_json_array one_dfa ds

(* ---------- Temporary DFA ---------- *)

let temporary_dfa_viewer (ds : temporary_dfa list) : json =
  let one_dfa (d : temporary_dfa) =

    let raw_transitions =
      List.flatten (
        List.map (fun state ->
          List.map (fun letter ->
            let next_state = d.delta state letter in
            (state, next_state, letter)
          ) d.alphabet
        ) d.states
      )
    in

    let transitions =
      merge_transitions char_to_string raw_transitions
      |> List.map (fun (from_s, to_s, label) ->
          Printf.sprintf
            "{ \"from\": %s, \"to\": %s, \"label\": \"%s\" }"
            (temp_state_to_json from_s)
            (temp_state_to_json to_s)
            label)
    in

    Printf.sprintf
      {|{
        "type": "temporary_dfa",
        "states": %s,
        "alphabet": %s,
        "transitions": [%s],
        "initial": %s,
        "final": %s
      }|}
      (list_to_json_array temp_state_to_json d.states)
      (list_to_json_array (fun c -> "\"" ^ char_to_string c ^ "\"") d.alphabet)
      (String.concat ", " transitions)
      (temp_state_to_json d.initial)
      (list_to_json_array temp_state_to_json d.final)
  in
  list_to_json_array one_dfa ds

(* ---------- NFA ---------- *)

let nfa_viewer (ns : nfa list) : json =
  let one_nfa (n : nfa) =
    let states = List.init n.nb_states (fun i -> i) in

    let raw_transitions =
      List.flatten (
        List.map (fun state ->
          List.flatten (
            List.map (fun letter ->
              let next_states = n.delta state letter in
              List.map (fun next_state ->
                (state, next_state, letter)
              ) next_states
            ) n.alphabet
          )
        ) states
      )
    in

    let label_to_string = function
      | Eps -> "ε"
      | Let c -> char_to_string c
    in

    let transitions =
      merge_transitions label_to_string raw_transitions
      |> List.map (fun (from_s, to_s, label) ->
          Printf.sprintf
            "{ \"from\": %d, \"to\": %d, \"label\": \"%s\" }"
            from_s to_s label)
    in

    Printf.sprintf
      {|{
        "type": "nfa",
        "states": %s,
        "alphabet": %s,
        "transitions": [%s],
        "initial": %d,
        "final": %s
      }|}
      (list_to_json_array string_of_int states)
      (list_to_json_array letter_to_json n.alphabet)
      (String.concat ", " transitions)
      n.initial
      (list_to_json_array string_of_int n.final)
  in
  list_to_json_array one_nfa ns

(* ---------- Store ---------- *)

let store (a : automaton) : unit =
  match a with
  | DFA d ->
      write "dfa.json" (dfa_viewer [d])
  | TempDFA d ->
      write "temp_dfa.json" (temporary_dfa_viewer [d])
  | NFA n ->
      write "nfa.json" (nfa_viewer [n])
  | MinNFA (n, alphabet) ->
      write "nfa.json" (nfa_viewer [mini_to_nfa n alphabet])
