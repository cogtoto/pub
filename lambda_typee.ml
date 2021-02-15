(* inference de type *)
type ltype = 
  | Int of int
  | Var of string
  | Fleche of ltype*ltype

  