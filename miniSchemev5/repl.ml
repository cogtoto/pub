open List
open Printf
open Scheme_exp 

exception Erreur of string
exception PasTrouve of string
exception PrintExpressionErreur

let car = function
 | Paire(x,y) -> !x
 | _ -> raise (Erreur "car")

let cdr = function
| Paire(x,y) -> !y
| _ -> raise (Erreur "cdr")

let caar exp = car (car exp)
let cdar exp = cdr (car exp)
let cadar exp = car (cdr (car exp))
let cadr exp = car (cdr exp)
let caddr exp = car (cdr (cdr exp))
let cadddr exp = car (cdr (cdr (cdr exp)))
let cddr exp = cdr (cdr exp)

let set_car a b  =
  match a with
   Paire (x,y) -> x:= b
   |_ -> raise (Erreur "set_car")

let set_cdr a b  =
  match a with
   Paire (x,y) -> y:=b
   |_ -> raise (Erreur "set_cdr")

let ints_to_into  = function
  | Entier n -> n
  | _ -> raise (Erreur " ints_to_into")

let bools_to_boolo = function
  | Booleen false -> false
  |_ -> true

let rec print_scheme e = (* print exp scheme *)
  let rec is_list e =
    match e with | Nil -> true | Paire (a, b) -> is_list !b | _ -> false in
  let rec print_list l =
    if l=Nil then print_string ""
    else (print_scheme (car l) ; print_string " "; print_list (cdr l))
  in 
  let print_pair p = (print_scheme (car p) ; print_string " . "; print_scheme (cdr p))
  in
  match e with
  | Entier n -> print_int n
  | Symbole s -> print_string s
  | Mot s -> print_string s
  | Nil -> print_string "()"
  | Booleen bo -> if bo then print_string "#t" else print_string "#f"
  | Paire (a, b) ->
    (print_string "(";
     if is_list e then print_list e else print_pair e;
     print_string ")")
  | Closure _ -> print_string "procedure#"

let rec print_scheme2 = function (* print exp scheme avec la représentation type CAML exp*)
  | Entier n -> (print_string "Entier(" ; print_int n ; print_string ")")
  | Symbole s -> (print_string "Symbole(" ; print_string s ; print_string ")")
  | Mot s -> (print_string "Mot(" ; print_string s ; print_string ")")
  | Booleen bo -> (print_string "Boolean(" ; if bo then print_string "#t" else print_string "#f" ;print_string ")")
  | Nil -> print_string "Nil"
  | Paire(a,b) -> ( print_string "Paire(" ; print_scheme2 !a ; print_string "," ; print_scheme2 !b; print_string ")")
  | _ -> raise (Erreur "print_scheme2") 

let rec scheme2string = function (* convert exp scheme to string avec la représentation type CAML exp*)
  | Entier n ->  "Entier(" ^ string_of_int n ^  ")"
  | Symbole s ->  "Symbole(" ^ s ^ ")"
  | Mot s -> "Mot(" ^ s ^  ")"
  | Booleen bo ->  "Boolean(" ^ if bo then  "#t" else  "#f" ^ ")"
  | Nil -> "Nil"
  | Paire(a,b) ->  "Paire(" ^ scheme2string !a ^  "," ^ scheme2string !b ^  ")"
  | _ -> raise (Erreur "scheme2string") 

  let rec strings_to_stringo = function 
  | Mot m -> m
  | Symbole m -> m 
  | Paire (a,b) ->  "Paire(" ^ strings_to_stringo !a ^ "," ^ strings_to_stringo !b ^ ")"
  | Booleen b -> "Booleen"
  | Nil -> "Nil"
  | Entier _ ->  raise (Erreur " strings_to_stringo entier")
  | Closure _ -> raise (Erreur " strings_to_stringo closure")
  
  
(* eval fonction, the big one*)
let rec eval ex env =
  match ex with 
  | Entier _ | Boolean _ ->  ex
  | Nil -> Nil
  | Lambda (par, exp) -> (fun par -> ex)
  | App (operator, operand1, operand2) -> operator operand1 operand2
  

  

let rec repl lexbuf env =
  begin
    print_string ">> "; flush stdout ;
    try
      let exp = Schemeyacc.main Schemelex.token lexbuf in
       (
      (* print_scheme exp ; print_string "\n"; 
       print_string (scheme2string exp) ; print_string "\n" ;
       print_string (ast2string (buildast exp)); print_string "\n"; *)  
       print_scheme (eval exp env) ; print_string "\n" ;
       repl lexbuf env
       )
    with
    | PasTrouve s -> (print_string "Variable non liée: "; print_string s; print_string "\n";flush stdout; repl lexbuf env)
    | Schemeyacc.Error -> (print_string "Parsing erreur\n"; flush stdout; repl lexbuf env)
    | Erreur s -> (print_string "Erreur évaluation "; print_string s ; print_string "\n"; flush stdout; repl lexbuf env)
    | Schemelex.Unexpected_token  -> (print_string "Erreur lexing \n"; flush stdout; repl lexbuf env)
  end

let main = let lexbuf = Lexing.from_channel stdin in repl lexbuf env          

