open Stack
type exp =
  | Booleen of bool
  | Symbole of string
  | Mot of string
  | Entier of int
  | Nil
  | Paire of exp ref * exp ref
  | Closure of string list * ast list * (env ref)
  and ast =
  | Atom of exp
  | Var of string
  | If of ast * ast * ast
  | Cond of (ast * ast) list
  | And of ast list
  | Or of ast list
  | Call of ast * ast list
  | Call0 of ast    (* procedure sans argument *)
  | Lambda of string list * ast list   
  | Let of (string * ast) list * ast list
  | Letrec of (string * ast) list * ast list
  | Define of string * ast
  | Begin of ast list
  | Apply of ast * ast list
  | Quote of exp
and env = (string * exp) list

type elt = 
   | CONST of int
   | ACCESS  of string
   | ADD
   | SUB
   | CLOSURE of code
   | APPLY
   | RETURN 
and code = elt list

exception Erreur of string
exception PasTrouve of string

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

let test = Paire (ref (Symbole "-"), ref (Paire (ref (Entier 5), ref(Paire (ref (Entier 6), ref Nil)))));;

let test2 = Paire (ref (Symbole "-"), ref (Paire (ref (Symbole "a"), ref(Paire (ref (Entier 6), ref Nil)))));;

let env = ref [("a", Entier(1976)); ("b", Paire(ref (Entier 1), ref (Paire (ref (Entier 2), ref Nil))))];;
let s = create();;

let rec lookup v env =
  match env with
  | [] -> raise (PasTrouve v) 
  | (a,b)::reste -> if v=a then b else lookup v reste

let op = car ;;
let opd1 l = car (cdr l);;
let opd2 l = car (cdr (cdr l)) ;;

let operator = function
  | Symbole "+" -> ADD
  | Symbole "-" -> SUB
  | _ -> raise (Erreur "operator")

let rec compile exp codesuivant =
  match exp with
  | Entier n -> CONST n :: codesuivant
  | Symbole s -> ACCESS s :: codesuivant
  | Paire _ ->
      begin
           if List.mem (op exp) [Symbole "+"; Symbole  "-"; Symbole "*"; Symbole "="] then  
           compile_app  (cdr exp)  (operator (car exp) :: codesuivant)
          else compile_lambda exp codesuivant
       end
  | _ -> raise (Erreur "compile")
and
compile_app args codesuivant =
  if args = Nil then codesuivant
  else  compile (car args) (compile_app  (cdr args) codesuivant) 
and compile_lambda exp codesuivant =
  let lvar = cadr exp 
  and corps = caddr exp
in CLOSURE(compile corps [RETURN]) :: codesuivant
  
let rec exe c =
  if (List.length c) = 0 then pop s
  else
  match (List.hd c) with
| ADD -> begin let Entier(n2) = pop s and Entier(n1) = pop s in  (push (Entier(n1+n2)) s ; exe (List.tl c)) end
| SUB -> begin let Entier(n2) = pop s and Entier(n1) = pop s in  (push (Entier(n1-n2)) s; exe (List.tl c)) end
| CONST n -> (push (Entier n) s ; exe (List.tl c))
| ACCESS sy -> ( push (lookup sy !env) s ; exe (List.tl c) )
| _ -> raise (Erreur "exe")   



let rec repl lexbuf env =
  begin
    print_string ">> "; flush stdout ;
    try
      let exp = Schemeyacc.main Schemelex.token lexbuf in
       (
      (* print_scheme exp ; print_string "\n";  
       print_string (scheme2string exp) ; print_string "\n" ;
       print_string (ast2string (buildast exp)); print_string "\n";  *)
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