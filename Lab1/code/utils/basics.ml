(**
  basics.ml --- Some basic but very usefull functions
*)

(* Equality function between two arrays *)
let array_equal eq a b =
  (* Useful function to check if two arrays a and b are equal according to the comparative function eq *)

  let n = Array.length a in
  n = Array.length b &&
  let rec aux i =
    if i = n then true
    else if eq a.(i) b.(i) then aux (i + 1)
    else false
  in aux 0;;

(* Deduplicating function *)
let dedup lst =
  let rec aux seen = function
    | [] -> List.rev seen
    | x :: xs ->
        if List.mem x seen then aux seen xs
        else aux (x :: seen) xs
  in
  aux [] lst

(* String to char list *)
let string_to_char_list (s : string) =
  List.of_seq (String.to_seq s)

(* Char list to string *)
let char_list_to_string lst =
  String.of_seq (List.to_seq lst)


(* JSON Helpers *)

let list_to_json_array conv lst =
  "[" ^ (String.concat ", " (List.map conv lst)) ^ "]"

let state_to_json s = string_of_int s

let temp_state_to_json states =
  "[" ^ (String.concat ", " (List.map string_of_int states)) ^ "]"

let write (name_file : string) content : unit =
  let oc = open_out ("viewer/" ^ name_file) in
  output_string oc content;
  close_out oc

let launch_server (port:int) (time:int) =
  let cmd =
    if Sys.os_type = "Win32" then
      Printf.sprintf
        "powershell -Command \"$p = Start-Process python -ArgumentList '-m http.server %d' -PassThru -WindowStyle Hidden; Start-Sleep -Seconds %d; Stop-Process -Id $p.Id\""
        port time
    else
      Printf.sprintf
        "( python3 -m http.server %d > /dev/null 2>&1 & pid=$!; sleep %d; kill $pid ) &"
        port time
  in
  ignore (Sys.command cmd)
;;


let bash (cmd : string) = ignore (Sys.command cmd) 