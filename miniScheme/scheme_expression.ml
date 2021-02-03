type lobject =
  | Entier of int
  | Booleen of bool
  | Symbole of string
  | Nil
  | Paire of lobject * lobject
	| Primitive of string * (lobject list -> lobject)
	| Quote of value      
	| Closure of name list * exp *  env   
	 
and value = lobject
and name = string
and exp =
  | Literal of value
  | Var of name
  | If of exp * exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Call of exp * exp list
	| Lambda of name list * exp    
	| Let of (name * exp) list * exp  
  | Defexp of def

and def =
  | Val of name * exp
	| Exp of exp

and env = (string * lobject) list
