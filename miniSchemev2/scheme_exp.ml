type exp =
  | Booleen of bool
  | Symbole of string
  | Mot of string
  | Entier of int
  | Nil
  | Paire of exp * exp
  | Closure of string list * ast * env
and ast =
  | Atom of exp
  | Var of string
  | If of ast * ast * ast
  | Cond of (exp * exp) list
  | And of ast * ast
  | Or of ast * ast
  | Call of ast * ast list
  | Call0 of ast    (* procedure sans argument *)
  | Lambda of string list * ast    
  | Let of (string * ast) list * ast  
  | Define of string * ast
  | Begin of ast list
  | Apply of ast * ast list
  | Quote of exp
and env = (string * exp) list

