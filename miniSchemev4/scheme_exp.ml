type exp =
  | Booleen of bool
  | Symbole of string
  | Mot of string
  | Entier of int
  | Nil
  | Paire of exp ref * exp ref
  | Closure of string list * ast list * (env ref)
  | CLOS of code * exp list list (* !!! used only for compilation !!!  *) 
  
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

and elt = 
   | CONST of int
   | ACCESS  of int array
   | ADD
   | SUB
   | CMP
   | CLOSURE of code
   | JSR
   | RTS 
   | NIL
   | ARG
   | BRANCH of code*code
and code = elt list

open Stack

type enve = (*environnement execution*)
 ENVEXE of exp list list  * exp list list  * code   

exception Erreur of string

let car = function
| Paire(x,y) -> !x
| _ -> raise (Erreur "car")

let cdr = function
| Paire(x,y) -> !y
| _ -> raise (Erreur "cdr")
