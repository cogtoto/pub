open List
open Printf
open Scheme_exp 

exception Erreur of string
exception PasTrouve of string
exception PrintExpressionErreur

let env = ref [("a", Entier(1976)); ("b", Paire(ref (Entier 1), ref (Paire (ref (Entier 2), ref Nil))))]

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

let rec lookup v env =
  match env with
  | [] -> raise (PasTrouve v) 
  | (a,b)::reste -> if v=a then b else lookup v reste

let bind (n, v, e) = (n, v) :: e

let etend ns vs env = 
  try 
    List.fold_left2 (fun acc n v -> bind (n, v, acc)) env ns vs
  with Invalid_argument(_) -> raise (Erreur "etend invalid arguments")
(*
List.fold_left2 f a [b1; ...; bn] [c1; ...; cn] is f (... (f (f a b1 c1) b2 c2) ...) bn cn.
Raises Invalid_argument if the two lists are determined to have different lengths.
*)

let modify_env (var,valeur) =
  let rec aux var valeur env =
  match env with
   | [] -> raise (PasTrouve var)
   | (a,b)::reste -> if a = var then (var, valeur)::reste else (a,b)::(aux var valeur reste)
in env:= (aux var valeur !env) 

let add_env (var,valeur) = 
  try
    modify_env (var,valeur) 
  with PasTrouve(_) ->  env := (var,valeur)::!env

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
  | Nil -> print_string ""
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
  
let rec buildast exp = 
  match exp with 
  | Booleen b -> Atom (Booleen b)
  | Symbole s -> Var s
  | Mot m -> Atom (Mot m)
  | Entier e -> Atom (Entier e)
  | Nil -> Atom (Nil)
  | Paire(a,b) -> build_call exp
  | _ -> raise ( Erreur "build_ast") 

  and build_call exp =
  match (car exp) with
        | Symbole "if" -> If(buildast (cadr exp),  buildast (caddr exp), buildast (cadddr  exp))
        | Symbole "or" -> Or(paire2list (cdr exp))
        | Symbole "and" -> And(paire2list (cdr exp))
        | Symbole "lambda" -> Lambda((paire2stringl (cadr exp)), buildast (caddr exp))
        | Symbole "define" -> 
                begin 
                   match (cadr exp) with
                    | Symbole(s) -> Define (s, buildast (caddr exp))
                    | _ -> raise (Erreur "build_call define")
                end
        | Symbole "let" -> Let((pairelet (cadr exp)), buildast (caddr exp))
        | Symbole "apply" -> Apply(buildast (cadr exp), paire2list (cddr exp))
        | Symbole "begin" -> Begin(paire2list (cdr exp))
        | Symbole "quote" | Symbole "'" -> Quote (cdr exp)
        | Symbole "cond" -> Cond (condbuild (cdr exp))
                        
        | _ -> if ((cdr exp) = Nil) then Call0 (buildast (car exp))
               else Call(buildast (car exp), paire2list (cdr exp))

and condbuild exp =
   if exp = Nil then []
   else ( buildast (caar exp), buildast (cadar exp) ) :: condbuild (cdr exp)

 and pairelet exp =
 if   exp=Nil then [] 
 else 
    match (caar exp) with
    |Symbole(s) -> (s, buildast (cadar exp)) :: pairelet (cdr exp)
    | _ -> raise (Erreur "pairelet")

and paire2stringl exp =
  if   exp=Nil then [] 
  else match (car exp) with
  | Symbole(s) -> s :: paire2stringl (cdr exp)
  | _ -> raise (Erreur "paire2stringl") 

and paire2list expl =
  if expl=Nil then []
  else buildast (car expl) :: paire2list (cdr expl)
  
(* print expression AST *)
let rec ast2string = function
  | Call (exp1, expl) -> "Call(" ^ ast2string exp1 ^ ", [" ^ expl2string expl ^ "])"
  | Apply(fn, expl) -> "Apply(" ^ast2string fn ^ ",[" ^expl2string expl ^ "])"
  | Call0 (exp) -> "Call0(" ^ ast2string exp ^ ")"
  | Quote (exp) -> "Quote(" ^ scheme2string exp ^")"
  | Cond (exp) -> "Cond()" 
  | If (exp1, exp2, exp3) -> "If(" ^ ast2string exp1 ^ "," ^ ast2string exp2 ^ "," ^ ast2string exp3 ^ ")"
  | And (exp) -> "And(" ^ expl2string exp  ^  ")"
  | Or (exp) -> "Or(" ^ expl2string exp  ^  ")"
  | Lambda (sl, exp) -> "Lambda([" ^ list2string sl ^ "]," ^ ast2string exp ^ ")"
  | Atom(Entier (n)) ->  "Atom(Entier " ^  (string_of_int n) ^ ")"
  | Atom(Booleen (b)) -> "Atom(Booleen " ^ (string_of_bool b) ^ ")" 
  | Atom(Symbole s) -> "Atom(Symbole " ^ s ^")"
  | Atom(Mot s) -> "Atom(Mot " ^ s ^")"
  | Define(s,exp) -> "Define(" ^ s ^ "," ^ (ast2string exp) ^ ")"
  | Let (s_exp_l, exp) -> "Let([" ^ (string_exp_liste s_exp_l) ^ "])," ^ ast2string exp ^ ")" 
  | Begin (corps) -> "Begin([" ^ expl2string corps ^ "])"
  | Var s -> "Var " ^ s 
  | _ -> raise (Erreur "ast2string")  
and expl2string = function
  | [] -> "]" 
  | a::[] -> ast2string a 
  | a::b -> ast2string a ^ ";" ^ expl2string b
and string_exp_liste = function
  | [] -> "]"
  | (s,e)::[] -> "(" ^ s ^ "," ^ (ast2string e) ^ ")"
  | (s,e)::suite -> "(" ^ s ^ "," ^ (ast2string e) ^ ");" ^ (string_exp_liste suite) 
and  list2string = function
  | [] -> "]"
  | a::[] -> a 
  | a::b -> a ^ ";" ^ list2string b 
and closure2string = function
  | Closure (l, exp, _) -> "Closure([" ^ list2string l ^ "]," ^ ast2string exp ^ ")" 
  | _ -> raise (Erreur " closure2string")
   
let rec replace_name l name = 
  match l with
    | Var s | Atom (Symbole s) -> if (s=name) then (Var "ffrec") else (Var s)
    | Atom x -> Atom x 
    | Or (exp) -> Or (replace_list exp name)
    | And (exp) -> And (replace_list exp name)
    | If (exp1, exp2, exp3) -> If ((replace_name exp1 name),(replace_name  exp2 name), (replace_name exp3 name)) 
    | Lambda (sl, exp) -> Lambda (sl, (replace_name exp name)) 
    | Call (exp1, exp2l) -> Call ((replace_name exp1 name), (replace_list exp2l name)) 
    | Quote exp -> Quote exp
    | Begin exp -> Begin (replace_list exp name)
    | _ -> raise ( Erreur "replace_name") 
and replace_list expl name = 
  match expl with
    |[] -> []
    | a::b -> (replace_name a name) :: replace_list b name 

  
(* eval fonction, the big one*)
let rec eval ex env =
  match ex with 
  | Atom (Entier n) -> Entier n
  | Atom (Booleen b) -> Booleen b
  | Atom (Symbole s) -> Symbole s
  | Atom (Mot s) -> Mot s
  | Atom (Nil) -> Nil
  | Var s -> lookup s env
  | Quote e -> e
  | If (a,b,c) | Call (Var "if", [a;b;c] )-> if bools_to_boolo (eval a env) then (eval b env) else (eval c env)
  | Cond l -> eval_cond l env
  | And (exp) -> Booleen (fold_left (&&) true (map bools_to_boolo (eval_liste exp env)))
  | Or (exp) -> Booleen (fold_left (||) false (map bools_to_boolo (eval_liste exp env)))
  | Define (v, exp) ->  let r =  eval exp env in (add_env (v, r)) ; r 
  | Lambda (vl, exp) -> Closure  (vl, exp, env)
  | Call (Var "+", opl) -> Entier(fold_left (+) 0 (map ints_to_into (eval_liste opl env)))
  | Call (Var "-", [op1; op2]) -> Entier(ints_to_into (eval op1 env) - ints_to_into (eval op2 env))
  | Call (Var "*", opl) -> Entier(fold_left ( * ) 1 (map ints_to_into (eval_liste opl env)))
  | Call (Var "=", [op1; op2]) -> Booleen (ints_to_into (eval op1 env) = ints_to_into (eval op2 env))
  | Call (Var "<", [op1; op2]) -> Booleen (ints_to_into (eval op1 env) < ints_to_into (eval op2 env))
  | Call (Var ">=", [op1; op2]) -> Booleen (ints_to_into (eval op1 env) >= ints_to_into (eval op2 env))
  | Call (Var "equal?", [op1; op2]) -> begin try Booleen ((strings_to_stringo (eval op1 env)) = (strings_to_stringo (eval op2 env))) 
                                       with Erreur(_) -> Booleen(false) end
  | Call (Var "not", [op1]) -> Booleen (not(bools_to_boolo(eval op1 env)))
  | Call (Var "env", [Var s]) -> Paire (ref (Symbole s), ref (Symbole (closure2string (lookup s env))))
  | Call0 (Var "env") -> eval_env env
  | Call (Var "cons", [op1; op2]) -> Paire (ref (eval op1 env), ref (eval op2 env))
  | Call (Var "list", l) -> create_liste l env
  | Call (Var "car", [l]) -> begin match (eval l env) with (Paire(a,b)) -> !a  | _ -> Nil end
  | Call (Var "cdr", [l]) -> begin match (eval l env)  with (Paire(a,b)) -> !b  |_ -> Nil end
  | Call (Var "set-car!", [a;b]) -> begin set_car (eval a env) (eval b env); Nil end 
  | Call (Var "set-cdr!", [a;b]) -> begin set_cdr (eval a env) (eval b env); Nil end
  | Call (Var "definerec", [Var name; exp]) -> 
     eval (Define(name, (Lambda (["any"], Call((Call (Var "y", [Lambda(["ffrec"],(replace_name exp name))]), [Var "any"])))))) env
  | Call0 (Var "read") -> let ic = open_in "minischeme.scm" in let lexbuf = Lexing.from_channel ic in lecture_file env lexbuf ic

  | Call0 (Var "lecture") -> let ic = stdin in let lexbuf = Lexing.from_channel ic in lecture env lexbuf ic
  
  | Call (Var "print", [Atom(Entier (s)) ]) -> (print_int s ; Nil)
  | Call (Var "print", [Atom(Mot (s)) ]) | Call (Var "print", [Atom(Symbole (s)) ]) | Call (Var "print", [Var s ])
         -> (print_string s ; Nil)
  | Call0 (Var "eol") -> (print_string "\n" ; Nil)
  | Call (Var "symbol?", [l]) -> begin match (eval l env) with Symbole(_)  -> Booleen(true) | _ -> Booleen false end
  | Call (Var "number?", [l]) -> begin match (eval l env) with Entier(_) -> Booleen(true) | _ -> Booleen false end
  | Call (Var "string?", [l]) -> begin match (eval l env) with Mot(_) -> Booleen(true) |    _ -> Booleen false end
  | Call (Var "boolean?", [l]) -> begin match (eval l env) with Booleen(_) -> Booleen(true) |  _ -> Booleen false end
  | Call (Var "null?", [l]) -> begin match (eval l env) with Nil -> Booleen(true) | _ -> Booleen false end
  | Call (op, operandl) -> invoke (eval op env) (eval_liste operandl env) env
  | Apply (fn, [Quote(largs)]) -> invoke (eval fn env) (exp2expl largs) env
  | Apply (fn, [arg]) -> invoke (eval fn env) (exp2expl (eval arg env)) env
  | Call0 (op) -> begin match (eval op env) with Closure(_, exp, env) -> (eval exp env)  | _ -> raise (Erreur " eval Call0") end
  | Let (l, c) -> eval (Call ( Lambda ((map fst l), c), (map snd l))) env 
  | Begin expl -> eval_begin expl env
  | Apply(_) -> raise (Erreur " apply ")
  | _ -> raise (Erreur "eval")
and invoke op varl env =
  match op with
  | Closure (vl, exp, envclo) -> eval exp (etend vl varl env) (* liaison dynamique avec env et non envclo *)
  | _ -> raise (Erreur " eval Invoke")
and exp2expl = function
  | Nil -> [] 
  | Paire(a,b) -> !a::(exp2expl !b) 
  | _ -> raise (Erreur "exp2expl")
and create_liste l env =
match l with
  | [] -> Nil
  | a::[] -> Paire (ref (eval a env), ref Nil)
  | a::b -> Paire (ref (eval a env), ref (create_liste b env))
and eval_liste l env =
  match l with
  | [] -> []
  | h::t ->  (eval h env) :: (eval_liste t env)
and eval_cond l env =
 match l with
  | (Var "else", b)::[] -> eval b env 
  | (a,b)::reste -> if (bools_to_boolo (eval a env)) then (eval b env) else eval_cond reste env
  | _ -> raise (Erreur "eval_cond") 
and lecture_file env lexbuf ic =
  begin
  print_string "<<Lecture fichier lib_dyn.scm>> \n"; 
  try
  let exp = Schemeyacc.main Schemelex.token lexbuf in
    (print_scheme exp ; print_string "\n" ; print_scheme (eval (buildast exp) env) ; print_string "\n" ;lecture_file env lexbuf ic)
  with Parsing.Parse_error | Schemelex.Eof->  (close_in ic; Nil)
  end
and lecture env lexbuf ic =
  begin
  print_string ">>>>> "; flush stdout ;
  Schemeyacc.main Schemelex.token lexbuf 
  end
  
and eval_begin expl env =
  match expl with
    | [] -> Nil
    | a::[] -> eval a env
    | a::b -> begin eval a env; eval_begin b env  end
and eval_env = function
    | [] -> Nil
    | (s,v)::b ->  Paire(ref (Paire(ref (Symbole s), ref v)), ref(eval_env b))

let rec repl lexbuf env =
  begin
    print_string ">> "; flush stdout ;
    try
      let exp = Schemeyacc.main Schemelex.token lexbuf in
       (
      (* print_scheme exp ; print_string "\n";  
       print_string (scheme2string exp) ; print_string "\n" ;
       print_string (ast2string (buildast exp)); print_string "\n"; *)
       print_scheme (eval (buildast exp) !env) ; print_string "\n" ;
       repl lexbuf env
       )
    with
    | PasTrouve s -> (print_string "Variable non liée: "; print_string s; print_string "\n";flush stdout; repl lexbuf env)
    | Schemeyacc.Error -> (print_string "Parsing erreur\n"; flush stdout; repl lexbuf env)
    | Erreur s -> (print_string "Erreur évaluation "; print_string s ; print_string "\n"; flush stdout; repl lexbuf env)
    | Schemelex.Unexpected_token  -> (print_string "Erreur lexing \n"; flush stdout; repl lexbuf env)
  end

let main = let lexbuf = Lexing.from_channel stdin in repl lexbuf env          

