type value= 
  | Entier of int
  | Booleen of bool
  | Symbole of string
  | Mot of string
  | Nil
  | Paire of value * value
  | Closure of string list * exp * env
  | Quote of exp
and exp =
  | Literal of value
  | Var of string
  | If of exp * exp * exp
  | Cond of (exp * exp) list
  | And of exp * exp
  | Or of exp * exp
  | Call of exp * exp list
  | Call0 of exp    (* procedure sans argument *)
	| Lambda of string list * exp    
  | Let of (string * exp) list * exp  
  | Define of string * exp
  | Begin of exp list
and env = (string * value) list
  
