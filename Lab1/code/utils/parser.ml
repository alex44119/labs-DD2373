(** 
  parser.ml --- Parser implementation and pretty printer

  parser : string -> letter reg error_option

  pretty_printer : letter reg error_option -> string

  raw_printer : letter reg error_option -> string
*)

open Alphabet
open Reg

(* Some helpers *)

type 'a error_option =
  | Value of 'a
  | Error of string

let is_prefix_at (s : string) (i : int) (prefix : string) : bool =
  let n = String.length s in
  let m = String.length prefix in
  i + m <= n && String.sub s i m = prefix

let reserved_ascii (c : letter) : bool =
  match c with
  | '(' | ')' | '|' | '*' | '+' | '?' | '.' -> true
  | _ -> false

let can_start_atom (s : string) (i : int) : bool =
  i < String.length s &&
  (
    is_prefix_at s i eps ||
    match s.[i] with
    | '(' | '.' -> true
    | ')' | '|' | '*' | '+' | '?' -> false
    | c when not (reserved_ascii c) -> true
    | _ -> false
  )

let errorf msg = Error msg

(*
   Grammar implemented:

   regex     := alt
   alt       := concat ('|' alt)?
   concat    := postfix+
   postfix   := atom ( '*' | '+' | '?' )*
   atom      := "ε" | "." | literal | '(' alt ')'

   Special top-level rule:
   - the empty string parses as Eps

   Notes:
   - postfix operators are accepted repeatedly and interpreted left-to-right
   - '+' is desugared: r+  ==> Concat (r, Star r)
   - '?' is desugared: r?  ==> Or (r, Eps)
*)

let parser (input : string) : letter reg error_option =
  let len = String.length input in

  let rec parse_alt i =
    match parse_concat i with
    | Error msg -> Error msg
    | Value (left, j) ->
        if j < len && input.[j] = '|' then
          begin
            match parse_alt (j + 1) with
            | Value (right, k) -> Value (Or (left, right), k)
            | Error _ -> Error "expected an expression after '|'"
          end
        else
          Value (left, j)

  and parse_concat i =
    if not (can_start_atom input i) then
      Error "expected an expression"
    else
      match parse_postfix i with
      | Error msg -> Error msg
      | Value (first, j) -> parse_concat_tail first j

  and parse_concat_tail left i =
    if can_start_atom input i then
      match parse_postfix i with
      | Error msg -> Error msg
      | Value (right, j) -> parse_concat_tail (Concat (left, right)) j
    else
      Value (left, i)

  and parse_postfix i =
    match parse_atom i with
    | Error msg -> Error msg
    | Value (base, j) -> parse_postfix_tail base j

  and parse_postfix_tail r i =
    if i >= len then
      Value (r, i)
    else
      match input.[i] with
      | '*' ->
          parse_postfix_tail (Star r) (i + 1)
      | '+' ->
          parse_postfix_tail (Concat (r, Star r)) (i + 1)
      | '?' ->
          parse_postfix_tail (Or (r, Eps)) (i + 1)
      | _ ->
          Value (r, i)

  and parse_atom i =
    if i >= len then
      Error "unexpected end of input"
    else if is_prefix_at input i eps then
      Value (Eps, i + String.length eps)
    else
      match input.[i] with
      | '.' ->
          Value (Any, i + 1)

      | '(' ->
          if i + 1 < len && input.[i + 1] = ')' then
            Error "empty parentheses are not allowed"
          else
            begin
              match parse_alt (i + 1) with
              | Error msg -> Error msg
              | Value (r, j) ->
                  if j < len && input.[j] = ')' then
                    Value (r, j + 1)
                  else
                    Error "missing closing ')'"
            end

      | ')' ->
          Error "unexpected ')'"

      | '|' ->
          Error "unexpected '|'"

      | '*' | '+' | '?' ->
          Error "postfix operator without a preceding expression"

      | c ->
          if reserved_ascii c then
            Error "unexpected reserved character"
          else
            Value (Lit c, i + 1)
  in

  if input = "" then
    Value Eps
  else
    match parse_alt 0 with
    | Error msg -> Error msg
    | Value (r, i) ->
        if i = len then
          Value r
        else
          match input.[i] with
          | ')' -> Error "unexpected ')' at top level"
          | _ -> Error "unexpected trailing characters"

let pretty_printer (r : letter reg error_option) : string =
  let rec pp prec re =
    match re with
    | Eps ->
        "ε"

    | Any ->
        "."

    | Lit c ->
        String.make 1 c

    | Star r1 ->
        let s = pp 3 r1 in
        let s =
          match r1 with
          | Eps | Any | Lit _ | Star _ -> s
          | _ -> "(" ^ s ^ ")"
        in
        s ^ "*"

    | Concat (r1, r2) ->
        let s1 = pp 2 r1 in
        let s2 = pp 2 r2 in
        let s =
          s1 ^ s2
        in
        if prec > 2 then "(" ^ s ^ ")" else s

    | Or (r1, r2) ->
        let s1 = pp 1 r1 in
        let s2 = pp 1 r2 in
        let s =
          s1 ^ "|" ^ s2
        in
        if prec > 1 then "(" ^ s ^ ")" else s
  in
  match r with
  | Error msg ->
      "Error: " ^ msg
  | Value re ->
      pp 0 re

let raw_printer (r : letter reg error_option) : string =
  let rec print_reg v = match v with
    | Any -> "Any"
    | Eps -> "Eps"
    | Lit c -> "Lit('" ^ String.make 1 c ^ "')"
    | Concat (r1, r2) ->
        "Concat(" ^ print_reg r1 ^ ", " ^ print_reg r2 ^ ")"
    | Or (r1, r2) ->
        "Or(" ^ print_reg r1 ^ ", " ^ print_reg r2 ^ ")"
    | Star r ->
        "Star(" ^ print_reg r ^ ")"
  in
  match r with
  | Value v -> print_reg v
  | Error msg -> msg