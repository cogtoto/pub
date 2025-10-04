(* inference de type *)
open Terme

type ltype = 
  | Int 
  | Var of string
  | Fleche of ltype*ltype

type env = ltype list


let imprime ty = 
  let rec aux = function
   | Int  -> "entier"
   | Var v -> v
   | Fleche (t1,t2) -> aux t1 ^ " -> " ^ aux t2
in print_string (aux ty) ;;

let rec imprime_env = function
  | [] -> ()
  | a::b -> imprime a ; print_string " ; " ; imprime_env b ;;

let env1 = [Fleche (Int, Var "a"); Var "b"; Fleche (Int, Int)] in
 (imprime_env env1 ; print_newline) ;;

