   
let rec replace_name l name = 
  match l with
   | Var s -> if (s=name) then (Var "ffrec") else (Var s)
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
 | Atom (Entier n) -> Entier n
 | Atom (Booleen b) -> Booleen b
 | Atom (Symbole s) -> Symbole s
 | Atom (Mot s) -> Mot s
 | Atom (Nil) -> Nil
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
 | Call (Var "print", [Atom(Entier (s)) ]) -> (print_int s ; Nil)
 | Call (Var "print", [Atom(Mot (s)) ]) -> (print_string s ; Nil)
 | Call0 (Var "eol") -> (print_string "\n" ; Nil)
 | Call (Var "atom?", [l]) -> begin match l with Atom(_) | Var(_) -> Booleen(true) | _ -> Booleen false end
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
  let exp = Schemeyacc.main Schemelex.token lexbuf in
   (print_scheme exp ; lecture env lexbuf ic)
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
