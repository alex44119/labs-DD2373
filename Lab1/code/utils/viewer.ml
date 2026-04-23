(**
    viewer.ml --- Generates the JSON code to visualize the automata
*)

open Alphabet
open Automata
open Basics

type json = string

let letter_to_json = function
  | Eps -> "\"ε\""
  | Let(c) -> "\"" ^ String.make 1 c ^ "\""

(* Convert DFA to JSON *)
let dfa_viewer (ds : dfa list) : json =
  let one_dfa (d : dfa) =
    let states = List.init d.nb_states (fun i -> i) in
    let transitions = ref [] in

    List.iter (fun state ->
      List.iter (fun letter ->
        let next_state = d.delta state letter in
        transitions := (Printf.sprintf "{ \"from\": %d, \"to\": %d, \"label\": \"%c\" }"
                        state next_state letter) :: !transitions
      ) d.alphabet
    ) states;

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
      (list_to_json_array (fun c -> "\"" ^ String.make 1 c ^ "\"") d.alphabet)
      (String.concat ", " !transitions)
      d.initial
      (list_to_json_array string_of_int d.final)
  in
  list_to_json_array (fun d -> one_dfa d) ds


(* Convert temporary DFA to JSON *)
let temporary_dfa_viewer (ds : temporary_dfa list) : json =
  let one_dfa (d : temporary_dfa) =
    let transitions = ref [] in

    List.iter (fun state ->
      List.iter (fun letter ->
        let next_state = d.delta state letter in
        transitions := (Printf.sprintf "{ \"from\": %s, \"to\": %s, \"label\": \"%c\" }"
                        (temp_state_to_json state) (temp_state_to_json next_state) letter) :: !transitions
      ) d.alphabet
    ) d.states;

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
      (list_to_json_array (fun c -> "\"" ^ String.make 1 c ^ "\"") d.alphabet)
      (String.concat ", " !transitions)
      (temp_state_to_json d.initial)
      (list_to_json_array temp_state_to_json d.final)
  in
  list_to_json_array (fun d -> one_dfa d) ds

(* Convert NFA to JSON *)
let nfa_viewer (ns : nfa list) : json =
  let one_nfa (n : nfa) =
    let states = List.init n.nb_states (fun i -> i) in
    let transitions = ref [] in

    List.iter (fun state ->
      List.iter (fun letter ->
        let next_states = n.delta state letter in
        List.iter (fun next_state ->
          transitions := (Printf.sprintf "{ \"from\": %d, \"to\": %d, \"label\": %s }"
                          state next_state (letter_to_json letter)) :: !transitions
        ) next_states
      ) n.alphabet
    ) states;

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
      (String.concat ", " !transitions)
      n.initial
      (list_to_json_array string_of_int n.final)
  in
  list_to_json_array (fun n -> one_nfa n) ns


(* The viewer function *)

let store (a : automaton) : unit = 
  match a with
    |DFA(d)     -> write "dfa.json" (dfa_viewer [d])
    |TempDFA(d) -> write "temp_dfa.json" (temporary_dfa_viewer [d])
    |NFA(n)     -> write "nfa.json" (nfa_viewer [n])
    |MinNFA(n, alphabet)  -> write "nfa.json" (nfa_viewer [(mini_to_nfa n alphabet)])