(**
  test_parser.ml --- Parser tester code.
*)

open Utils.Parser

let test (s : string) : unit =
  let result = parser s in
  Printf.printf "input  : %S\n" s;
  Printf.printf "raw : %s\n" (raw_printer result);
  Printf.printf "output : %s\n" (pretty_printer result);
  Printf.printf "-----------------------------\n"

let () =
  let tests = [
    "";
    "a";
    "ε";
    ".";
    "ab";
    "a|b";
    "(a|b)*";
    "a+";
    "a?";
    "a*?";
    "a+*";
    "(ab)?*";
    "a(b|c)";
    "ab|c";
    "a|bc";
    "()";
    "|";
    "a|";
    "|a";
    "*a";
    "(a";
    "a)";
  ] in
  List.iter test tests