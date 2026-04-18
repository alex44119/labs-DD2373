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

