open List
open Printf
open Scheme_exp 

exception Erreur of string
exception PasTrouve of string
exception PrintExpressionErreur

let env = ref [("inc", Closure (["n"], Call(Var "+", [Var "n"; Literal (Entier 1)]), []) );
               ("y", Closure   (["h"],Lambda(["x"],Call(Call(Lambda(["f"],Lambda(["x"],Call(Call(Var "f", [Var "f"]), [Var "x"]))), 
               [Lambda(["f"],Call(Lambda(["g"],Call(Var "g", [Lambda(["x"],Call(Call(Var "f", [Var "f"]), [Var "x"]))])),
                [Var "h"]))]), [Var "x"])), []) ) ;
               ("cddr", Closure(["l"],Call(Var "cdr", [Call(Var "cdr", [Var "l"])]), [])) ;
               ("cadr", Closure(["l"],Call(Var "car", [Call(Var "cdr", [Var "l"])]), [])) 
               ]

let rec lookup v env =
  match env with
  | [] -> raise (PasTrouve v) 
  | (a,b)::reste -> if v=a then b else lookup v reste

(* print expression AST *)
let rec list2string = function
  | [] -> "]"
  | a::[] -> a 
  | a::b -> a ^ ";" ^ list2string b 
and closure2string = function
  | Closure (l, exp, _) -> "Closure([" ^ list2string l ^ "]," ^ exp2string exp ^ ")" 
  | _ -> raise (Erreur " print closure")
and exp2string = function
  | Call (exp1, expl) -> "Call(" ^ exp2string exp1 ^ ", [" ^ expl2string expl ^ "])"
  | If (exp1, exp2, exp3) -> "If(" ^ exp2string exp1 ^ "," ^ exp2string exp2 ^ "," ^ exp2string exp3 ^ ")"
  | And (exp1, exp2) -> "And(" ^ exp2string exp1 ^ "," ^ exp2string exp2 ^ ")"
  | Or (exp1, exp2) -> "Or(" ^ exp2string exp1 ^ "," ^ exp2string exp2 ^ ")"
  | Lambda (sl, exp) -> "Lambda([" ^ list2string sl ^ "]," ^ exp2string exp ^ ")"
  | Literal(Entier (n)) ->  "Literal(Entier " ^  (string_of_int n) ^ ")"
  | Literal(Booleen (b)) -> "Literal(Booleen " ^ (string_of_bool b) ^ ")" 
  | Literal(Symbole s) -> "Literal(Symbole " ^ s ^")"
  | Define(s,exp) -> "Define(" ^ s ^ "," ^ (exp2string exp) ^ ")"
  | Let (s_exp_l, exp) -> "Let([" ^ (string_exp_liste s_exp_l) ^ "," ^ exp2string exp ^ ")" 
  | Var s -> "Var " ^ s 
  | _ -> raise (Erreur " print exp" ) 
and expl2string = function
  | [] -> "]" 
  | a::[] -> exp2string a 
  | a::b -> exp2string a ^ ";" ^ expl2string b
and string_exp_liste = function
  | [] -> "]"
  | (s,e)::[] -> "(" ^ s ^ "," ^ (exp2string e) ^ ")"
  | (s,e)::suite -> "(" ^ s ^ "," ^ (exp2string e) ^ ");" ^ (string_exp_liste suite) 

let add_env (var,valeur) = env := (var,valeur)::!env

let bind (n, v, e) = (n, v) :: e
let etend ns vs env = List.fold_left2 (fun acc n v -> bind (n, v, acc)) !env ns vs
(*
List.fold_left2 f a [b1; ...; bn] [c1; ...; cn] is f (... (f (f a b1 c1) b2 c2) ...) bn cn.
Raises Invalid_argument if the two lists are determined to have different lengths.
*)

let ints_to_into  = function
 | Entier n -> n
 | _ -> raise (Erreur " ints_to_into")

let bools_to_boolo = function
  | Booleen false -> false
  |_ -> true

let strings_to_stringo = function 
  | Mot m -> m
  | Symbole m -> m 
  | _ -> raise (Erreur " strings_to_stringo")

  
let rec print_scheme e =
  let rec is_list e =
    match e with | Nil -> true | Paire (a, b) -> is_list b | _ -> false in
  let rec print_list l =
    match l with
    | Paire (a, Nil) -> print_scheme a
    | Paire (a, b) -> (print_scheme a; print_string " "; print_list b)
    | _ -> raise (Erreur " print_list") in
  let print_pair p =
    match p with
    | Paire (a, b) -> (print_scheme a; print_string " . "; print_scheme b)
    | _ -> raise (Erreur " print_pair")
  in
    match e with
    | Entier n -> print_int n
    | Symbole s -> print_string s
    | Mot s -> print_string s
    | Nil -> print_string ""
    | Booleen bo -> if bo then print_string "#t" else print_string "#f"
    | Paire (a, b) ->
        (print_string "(";
          if is_list e then print_list e else print_pair e;
          print_string ")")
    | Closure _ -> print_string "procedure "
    | Quote e -> print_string (exp2string e) 
    
let rec replace_name l name = 
  match l with
   | Var s -> if (s=name) then (Var "ffrec") else (Var s)
   | Literal v -> Literal v
   | Or (exp1, exp2) -> Or ((replace_name exp1 name),(replace_name  exp2 name))
   | And (exp1, exp2) -> And ((replace_name exp1 name),(replace_name  exp2 name))
   | If (exp1, exp2, exp3) -> If ((replace_name exp1 name),(replace_name  exp2 name), (replace_name exp3 name)) 
   | Lambda (sl, exp) -> Lambda (sl, (replace_name exp name)) 
   | Call (exp1, exp2l) -> Call ((replace_name exp1 name), (replace_list exp2l name)) 
   | _ -> raise ( Erreur "replace_name") 
and replace_list expl name = 
 match expl with
   |[] -> []
   | a::b -> (replace_name a name) :: replace_list b name 

(* eval fonction, the big one*)
let rec eval ex env =
  match ex with 
 | Literal (Entier n) -> Entier n
 | Literal (Booleen b) -> Booleen b
 | Literal (Symbole s) -> Symbole s
 | Literal (Mot s) -> Mot s
 | Literal Nil -> Nil
 | Literal (Quote e) -> begin match e with Var s -> Symbole s |_ -> Quote e end
 | Var s -> lookup s env
 | If (a,b,c) -> if bools_to_boolo (eval a env) then (eval b env) else (eval c env)
 | And (a,b) -> if bools_to_boolo (eval a env) && bools_to_boolo (eval b env) then Booleen true else Booleen false
 | Or (a,b) -> if bools_to_boolo (eval a env) || bools_to_boolo (eval b env) then Booleen true else Booleen false
 | Define (v, exp) ->  let r =  eval exp env in (add_env (v,r) ; r )
 | Lambda (vl, exp) -> Closure  (vl, exp, env)
 | Cond l -> eval_cond l env
 | Call (Var "+", opl) -> Entier(fold_left (+) 0 (map ints_to_into (eval_liste opl env)))
 | Call (Var "-", [op1; op2]) -> Entier(ints_to_into (eval op1 env) - ints_to_into (eval op2 env))
 | Call (Var "*", opl) -> Entier(fold_left ( * ) 1 (map ints_to_into (eval_liste opl env)))
 | Call (Var "=", [op1; op2]) -> Booleen (ints_to_into (eval op1 env) = ints_to_into (eval op2 env))
 | Call (Var "<", [op1; op2]) -> Booleen (ints_to_into (eval op1 env) < ints_to_into (eval op2 env))
 | Call (Var ">=", [op1; op2]) -> Booleen (ints_to_into (eval op1 env) >= ints_to_into (eval op2 env))
 | Call (Var "equal?", [op1; op2]) -> Booleen (strings_to_stringo (eval op1 env) = strings_to_stringo (eval op2 env))
 | Call (Var "env", [Var s]) -> Paire (Symbole s, Symbole (closure2string (lookup s env)))
 | Call0 (Var "env") -> eval_env env
 | Call (Var "cons", [op1; op2]) -> Paire ((eval op1 env), (eval op2 env))
 | Call (Var "list", l) -> create_liste l env
 | Call (Var "car", [l]) -> begin match (eval l env) with (Paire(a,b)) -> a | _ -> Nil end 
 | Call (Var "cdr", [l]) -> begin match (eval l env) with (Paire(a,b)) -> b | _ -> Nil end
 | Call (Var "definerec", [Var name; exp]) -> 
      eval (Define(name, (Lambda (["any"], Call((Call (Var "y", [Lambda(["ffrec"],(replace_name exp name))]), [Var "any"])))))) env
 | Call0 (Var "read") -> let ic = open_in "lib.scm" in let lexbuf = Lexing.from_channel ic in lecture env lexbuf ic
 | Call (Var "print", [Literal(Entier (s)) ]) -> (print_int s ; Nil)
 | Call (Var "print", [Literal(Mot (s)) ]) -> (print_string s ; Nil)
 | Call0 (Var "eol") -> (print_string "\n" ; Nil)
 | Call (Var "atom?", [l]) -> begin match l with Literal(_) | Var(_) -> Booleen(true) | _ -> Booleen false end
 | Call (Var "null?", [l]) -> begin match (eval l env) with Nil -> Booleen(true) | _ -> Booleen false end
 | Call (op, operandl) -> invoke (eval op env) (eval_liste operandl env)
| Call0 (op) -> begin match (eval op env) with Closure(_, exp, env) -> (eval exp env)  | _ -> raise (Erreur " eval Call0") end
 | Let (l, c) -> eval (Call ( Lambda ((map fst l), c), (map snd l))) env 
 | Begin expl -> eval_begin expl env
 | _ -> raise (Erreur " eval Begin")
and invoke op varl  =
  match op with
  | Closure (vl, exp, envclo) -> eval exp (etend vl varl (ref envclo))
  | _ -> raise (Erreur " eval Invoke")
and eval_cond l env = 
match l with
  | (Var "else", exp2)::_ -> (eval exp2 env)
  | (c, e)::[] -> if (bools_to_boolo (eval c env)) then (eval e env) else Booleen(false)
  | (c,e)::reste -> if (bools_to_boolo (eval c env)) then (eval e env) else (eval_cond reste env)
  | [] -> Booleen(false)
and create_liste l env =
match l with
  | [] -> Nil
  | a::[] -> Paire ((eval a env), Nil)
  | a::b -> Paire ((eval a env), create_liste b env)
and eval_liste l env =
  match l with
  | [] -> []
  | h::t ->  (eval h env) :: (eval_liste t env)
and lecture env lexbuf ic =
  begin
  print_string "<<Lecture fichier lib.scm>> \n"; 
  try
  let ast = Schemeyacc.main Schemelex.token lexbuf in
   (eval ast env ; lecture env lexbuf ic)
  with Parsing.Parse_error | Schemelex.Eof->  (close_in ic; Nil)
  end
and eval_begin expl env =
  match expl with
   | [] -> raise (Erreur "eval begin")
   | a::[] -> eval a env
   | a::b -> begin eval a env; eval_begin b env  end
and eval_env = function
   | [] -> Nil
   | (s,v)::b ->  Paire(Paire(Symbole s, v), eval_env b)

let rec repl lexbuf env =
  begin
  print_string ">> "; flush stdout ;
  try
      let ast = Schemeyacc.main Schemelex.token lexbuf in
        (print_scheme (eval ast !env) ; print_newline (); repl lexbuf env)
   with
     | PasTrouve s -> (print_string "Variable non liée: "; print_string s; print_string "\n";flush stdout; repl lexbuf env)
     | Schemeyacc.Error -> (print_string "Parsing erreur\n"; flush stdout; repl lexbuf env)
     | Erreur s -> (print_string "Erreur évaluation"; print_string s ; print_string "\n"; flush stdout; repl lexbuf env)
     | Schemelex.Unexpected_token  -> (print_string "Erreur lexing \n"; flush stdout; repl lexbuf env)
  end

let main = let lexbuf = Lexing.from_channel stdin in repl lexbuf env          

