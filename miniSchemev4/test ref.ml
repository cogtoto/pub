open List
open Printf
open Scheme_exp 

exception Erreur of string
exception PasTrouve of string
exception PrintExpressionErreur

let env = ref [("a", Entier(1976)); ("b", Entier(2000))] ;;


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

  let clo = Closure  (["v1"], Atom(Entier 2021), env) ;;
  add_env ("fonction", clo) ;;
