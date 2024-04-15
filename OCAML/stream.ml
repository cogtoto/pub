open Stream ;;
exception Erreur of string ;;

let s1 = of_string "(+ 1 (+ 31 0))" ;;

type token =
| Paro (* parenthèse ( *)
| Parf (* parenthèse ) *)
| Plus
| Int of int

let rec lire_entier s acc =
  try
  let current = next s in
  match current with
  | '0' .. '9' -> lire_entier s (acc * 10 + (int_of_char current) - 48)
  | ' ' | '(' | ')' -> acc
  | _-> raise (Erreur "lire_entier")
  with Stream.Failure -> acc
;;

let lex s = 
  let rec lex_aux s acc =
  try
    let current = next s in
  match current with
  |  '+' -> lex_aux s (Plus :: acc)
  |  '(' -> lex_aux s (Paro :: acc)
  |  ')' -> lex_aux s (Parf :: acc)
  |  '0' .. '9'  -> lex_aux s  (Int (lire_entier s ((int_of_char current)-48)) :: acc)
  |  ' ' -> lex_aux s acc
  |  _ -> raise (Erreur "lex") 
  with Stream.Failure -> List.rev acc
  in lex_aux s []
;;

let lire_lex s = 
  try
    let current = next s in
  match current with
  |  '+' -> Some Plus 
  |  '(' -> Some Paro
  |  ')' -> Some Parf
  |  '0' .. '9'  -> Some (Int (lire_entier s ((int_of_char current)-48)))
  |  _ -> None  
  with Stream.Failure -> None
;;

