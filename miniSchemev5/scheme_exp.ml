type exp =
  | Entier of int
  | Boolean of bool
  | Nil
  | Paire of exp ref * exp ref
  | Lambda of string list * exp list  
  | Apply of exp * exp list
  | Let of (string * exp) list * exp list
  | If of exp * exp * exp
 
