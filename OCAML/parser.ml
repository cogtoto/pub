open Stream 

let s=of_string "+ 1 + 5 82" ;;

type token =
| Plus
| Entier of int

let lex s = 
  let acc = ref [] in
  let buffer_int = ref 0 in
  let rec lex_aux s =
  begin
  try
    match next s with
    | ' ' -> acc := !acc @ [Entier !buffer_int]; buffer_int:=0; lex_aux s
    | '1' .. '9' as n -> buffer_int := (int_of_char n) + (10 * !buffer_int); lex_aux s
    | '+'  -> acc := !acc @ [Plus] ; lex_aux s
    | _ -> ()
  with Failure -> ()
  end
in (lex_aux s; !acc) ;;

lex s ;;