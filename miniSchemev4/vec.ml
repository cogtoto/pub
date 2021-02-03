open Array
open List
open Hashtbl

exception Erreur of string

let v = [|2004; 2006; 2008 |] ;;
let t = Array.make 10 "toto" ;;
let u = Array.make_matrix 5 5 0 ;;
let p = Array.of_list [1;2;3] ;;

type exp =
  | Booleen of bool
  | Symbole of string
  | Mot of string
  | Entier of int
  | Nil
  | Paire of exp ref * exp ref

type mystack = exp list

let push a s = a::s
let pop s = tl s ;;

push (Entier 3) [] ;;

let adresse var env_c =
  let rec aux lvar env_c p i =
    try
      match lvar with  
      | [] -> aux (hd env_c) (tl env_c) (1+p) 0
      | a::_ when a = var -> [|p; i|] 
      | _ -> aux (tl lvar) env_c p (i+1)
    with Failure tl -> raise (Erreur "adresse indefinie")
  in aux (hd env_c) (tl env_c) 0 0 ;;
  
let lire_env adr e = nth (nth e adr.(0)) adr.(1) ;;

adresse "v" [["x"; "y"]; ["u"; "v"]; ["r" ; "s";"w"]] ;;
