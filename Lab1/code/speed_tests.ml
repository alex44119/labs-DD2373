(**
  Speed tests / benchmark report.
*)

open Utils.Parser
open Utils.Nfa_cons
open Utils.Nfa_dfa
open Utils.Basics
open Utils.Automata
open Utils.Alphabet
open Utils.Dfa_minimization
open Utils.Simulation

(* You need gettimeofday *)
open Unix

(* ---------- Small timing helpers ---------- *)

let time_it f =
  let t0 = gettimeofday () in
  let res = f () in
  let t1 = gettimeofday () in
  (res, t1 -. t0)

let sum_float_list l =
  List.fold_left ( +. ) 0.0 l

let min_float_list = function
  | [] -> 0.0
  | x :: xs -> List.fold_left min x xs

let max_float_list = function
  | [] -> 0.0
  | x :: xs -> List.fold_left max x xs

type sim_stats = {
  total_time : float;
  avg_time   : float;
  min_time   : float;
  max_time   : float;
  accepted   : int;
  total      : int;
}

let benchmark_simulation automaton text_lines =
  let rec aux lines times accepted_count total_count =
    match lines with
    | [] ->
        let total_time = sum_float_list (List.rev times) in
        let times_rev = List.rev times in
        {
          total_time;
          avg_time = if total_count = 0 then 0.0 else total_time /. float_of_int total_count;
          min_time = min_float_list times_rev;
          max_time = max_float_list times_rev;
          accepted = accepted_count;
          total = total_count;
        }
    | line :: rest ->
        let input = string_to_char_list line in
        let (is_ok, dt) = time_it (fun () -> simulate automaton input) in
        aux
          rest
          (dt :: times)
          (if is_ok then accepted_count + 1 else accepted_count)
          (total_count + 1)
  in
  aux text_lines [] 0 0

let print_separator () =
  print_endline "------------------------------------------------------------------"

let print_metric name value =
  Printf.printf "%-40s %s\n" name value

let print_time_metric name seconds =
  Printf.printf "%-40s %.6f s\n" name seconds

let safe_ratio a b =
  if b = 0.0 then infinity else a /. b

(* ---------- Input ---------- *)

let global_t0 = gettimeofday ()

let lines = read_all_lines ()

let alphabet_unparsed, regexp_unparsed, text_lines =
  match lines with
  | a :: r :: inputs -> a, r, inputs
  | _ -> failwith "Error: input must contain at least 2 lines (alphabet + regex)"

let alphabet = string_to_char_list alphabet_unparsed

let regexp =
  let r = parser (".*(" ^ regexp_unparsed ^ ")") in
  match r with
  | Value a -> a
  | Error e -> failwith e

(* ---------- Construction timings ---------- *)

let (nfa, nfa_build_time) =
  time_it (fun () ->
    mini_to_nfa (nfa_cons regexp) (Eps :: List.map (fun x -> Let x) alphabet)
  )

let nfa_state_count = nfa.nb_states

let (temp_dfa, temp_dfa_time) =
  time_it (fun () -> nfa_temp_dfa nfa)

let (dfa, dfa_finalize_time) =
  time_it (fun () -> temp_dfa_dfa temp_dfa)

let dfa_state_count = dfa.nb_states

(* Keeping your original way of getting min_dfa *)
let (min_dfa, min_dfa_build_time) =
  time_it (fun () -> temp_dfa_dfa (dfa_temp_dfa dfa))

let min_dfa_state_count = min_dfa.nb_states

let total_automata_build_time =
  nfa_build_time +. temp_dfa_time +. dfa_finalize_time +. min_dfa_build_time

(* ---------- Simulation timings ---------- *)

let nfa_stats =
  benchmark_simulation (NFA nfa) text_lines

let dfa_stats =
  benchmark_simulation (DFA min_dfa) text_lines

let global_t1 = gettimeofday ()
let whole_program_time = global_t1 -. global_t0

(* ---------- Report ---------- *)

let () =
  print_endline "";
  print_separator ();
  print_endline "BENCHMARK REPORT";
  print_separator ();

  print_metric "Alphabet" alphabet_unparsed;
  print_metric "Regex" regexp_unparsed;
  print_metric "Number of tested lines" (string_of_int (List.length text_lines));
  print_endline "";

  print_separator ();
  print_endline "AUTOMATA CONSTRUCTION";
  print_separator ();
  print_time_metric "NFA construction time" nfa_build_time;
  print_time_metric "Temp DFA construction time" temp_dfa_time;
  print_time_metric "DFA finalization time" dfa_finalize_time;
  print_time_metric "Min DFA construction time" min_dfa_build_time;
  print_time_metric "Total automata build time" total_automata_build_time;
  print_endline "";

  print_separator ();
  print_endline "STATE COUNTS";
  print_separator ();
  print_metric "NFA states" (string_of_int nfa_state_count);
  print_metric "DFA states" (string_of_int dfa_state_count);
  print_metric "Min DFA states" (string_of_int min_dfa_state_count);
  Printf.printf "%-40s %.2f\n"
    "NFA / Min DFA state ratio"
    (safe_ratio (float_of_int nfa_state_count) (float_of_int min_dfa_state_count));
  print_endline "";

  print_separator ();
  print_endline "SIMULATION TIMES";
  print_separator ();
  print_time_metric "NFA total simulation time" nfa_stats.total_time;
  print_time_metric "NFA average time per line" nfa_stats.avg_time;
  print_time_metric "NFA min time for one line" nfa_stats.min_time;
  print_time_metric "NFA max time for one line" nfa_stats.max_time;
  print_metric "NFA accepted lines"
    (Printf.sprintf "%d / %d" nfa_stats.accepted nfa_stats.total);
  print_endline "";

  print_time_metric "Min DFA total simulation time" dfa_stats.total_time;
  print_time_metric "Min DFA average time per line" dfa_stats.avg_time;
  print_time_metric "Min DFA min time for one line" dfa_stats.min_time;
  print_time_metric "Min DFA max time for one line" dfa_stats.max_time;
  print_metric "Min DFA accepted lines"
    (Printf.sprintf "%d / %d" dfa_stats.accepted dfa_stats.total);
  print_endline "";

  print_separator ();
  print_endline "COMPARISON";
  print_separator ();
  Printf.printf "%-40s %.2f x\n"
    "NFA slower than Min DFA (total)"
    (safe_ratio nfa_stats.total_time dfa_stats.total_time);
  Printf.printf "%-40s %.2f x\n"
    "NFA slower than Min DFA (per line avg)"
    (safe_ratio nfa_stats.avg_time dfa_stats.avg_time);

  if nfa_stats.accepted = dfa_stats.accepted then
    print_metric "Acceptance check" "OK: NFA and Min DFA agree"
  else
    print_metric "Acceptance check" "WARNING: NFA and Min DFA differ";

  print_endline "";

  print_separator ();
  print_endline "WHOLE PROGRAM";
  print_separator ();
  print_time_metric "Total wall-clock time for whole file" whole_program_time;
  print_separator ();
  print_endline ""
