(*
  Main code for the project.
*)



(* Idea for main by ChatGPT *)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input_file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let dfa = build_dfa filename in

  (* Example: print number of states *)
  Printf.printf "DFA loaded with %d states\n" dfa.nb_states