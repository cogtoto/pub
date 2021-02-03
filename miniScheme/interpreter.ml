open List
open Printf
open Scheme_expression

exception Erreur
exception PasTrouve of string
exception TypeError of string
  
let rec lookup =
  function
  | (n, []) -> raise (PasTrouve n)
  | (n, (n', v) :: rst) -> if n = n' then v else lookup (n, rst)
  
let bind (n, v, e) = (n, v) :: e
  
let extend newenv oldenv =
  List.fold_right (fun (n, v) acc -> bind (n, v, acc)) newenv oldenv
  
let bindlist ns vs env =
  List.fold_left2 (fun acc n v -> bind (n, v, acc)) env ns vs
  
let rec print_scheme e =
  let rec is_list e =
    match e with | Nil -> true | Paire (a, b) -> is_list b | _ -> false in
  let rec print_list l =
    match l with
    | Paire (a, Nil) -> print_scheme a
    | Paire (a, b) -> (print_scheme a; print_string " "; print_list b)
    | _ -> raise Erreur in
  let rec print_parlist l =
    match l with
    | [] -> ()
    | car :: cdr -> (print_string car; print_string " "; print_parlist cdr) in
  let print_pair p =
    match p with
    | Paire (a, b) -> (print_scheme a; print_string " . "; print_scheme b)
    | _ -> raise Erreur
  in
    match e with
    | Entier n -> print_int n
    | Symbole s -> print_string s
    | Nil -> print_string "Nil"
    | Booleen bo -> if bo then print_string "#t" else print_string "#f"
    | Closure (par, body, envc) ->
        (printf "#<closure> :";
         print_string " <pars> ";
         print_parlist par;
         print_string " <beg body> ";
         print_string " <end body>")
    | Paire (a, b) ->
        (print_string "(";
         if is_list e then print_list e else print_pair e;
         print_string ")")
    | Quote _ -> ()
    | Primitive (_, _) -> ()
  
(* fonctions primitives*)
let prim_arith f l =
  match l with
  | [ Entier a; Entier b ] -> Entier (f a b)
  | _ -> raise (TypeError "primitive arithmetique")
  
let rec prim_list =
  function | [] -> Nil | car :: cdr -> Paire (car, (prim_list cdr))
  
let rec env_to_value =
  function
  | [] -> Nil
  | (var, Entier n) :: cdr ->
      Paire ((Paire ((Symbole var), (Entier n))), (env_to_value cdr))
  | (var, Symbole n) :: cdr ->
      Paire ((Paire ((Symbole var), (Symbole n))), (env_to_value cdr))
  | (var, Booleen n) :: cdr ->
      Paire ((Paire ((Symbole var), (Booleen n))), (env_to_value cdr))
  | (var, Closure (par, exp, env)) :: cdr ->
      Paire ((Paire ((Symbole var), (Closure (par, exp, env)))),
        (env_to_value cdr))
  | _ -> raise Erreur
  
let rec evalexp exp env =
  let evalapply f vs =
    match f with
    | Primitive (_, f) -> f vs
    | Closure (ns, e, clenv) -> evalexp e (bindlist ns vs clenv)
    | _ -> raise (TypeError "(apply procedure args") in
  let rec ev =
    function
    | Literal (Quote e) -> e
    | Literal l -> l
    | Var n -> lookup (n, env)
    | If (c, t, f) when (ev c) = (Booleen true) -> ev t
    | If (c, t, f) when (ev c) = (Booleen false) -> ev f
    | If _ -> raise (TypeError "(if bool e1 e2)")
    | And (c1, c2) ->
        (match ((ev c1), (ev c2)) with
         | (Booleen v1, Booleen v2) -> Booleen (v1 && v2)
         | _ -> raise (TypeError "(and bool bool)"))
    | Or (c1, c2) ->
        (match ((ev c1), (ev c2)) with
         | (Booleen v1, Booleen v2) -> Booleen (v1 || v2)
         | _ -> raise (TypeError "(or bool bool)"))
    | Lambda (parametres, expression) ->
        Closure (parametres, expression, env)
    | Let (bindings, body) ->
        let evbinding (n, e) = (n, (ev e))
        in evalexp body (extend (List.map evbinding bindings) env)
    | Call ((Var "plus"), ([ a; b ])) ->
        let a' = ev a and b' = ev b in prim_arith ( + ) [ a'; b' ]
    | Call ((Var "moins"), ([ a; b ])) ->
        let a' = ev a and b' = ev b in prim_arith ( - ) [ a'; b' ]
    | Call ((Var "mult"), ([ a; b ])) ->
        let a' = ev a and b' = ev b in prim_arith ( * ) [ a'; b' ]
    | Call ((Var "eq?"), ([ a; b ])) ->
        let a' = ev a and b' = ev b in Booleen (a' = b')
    | Call ((Var "pair"), ([ a; b ])) ->
        let a' = ev a and b' = ev b in Paire (a', b')
    | Call ((Var "list"), l) -> prim_list (List.map ev l)
    | Call ((Var "env"), _) -> env_to_value env
    | Call (e, es) -> evalapply (ev e) (List.map ev es)
    | Defexp d -> raise Erreur
  in ev exp
  
let evaldef def env =
  match def with
  | Val (n, e) -> let v = evalexp e env in (v, (bind (n, v, env)))
  | Exp e -> ((evalexp e env), env)
  
let eval ast env =
  match ast with | Defexp d -> evaldef d env | e -> ((evalexp e env), env)
  
let rec repl lexbuf env =
  (print_string ">> ";
   try
     let ast = Schemeyacc.main Schemelex.token lexbuf in
     let (result, env') = eval ast env
     in (print_scheme result; print_newline (); repl lexbuf env')
   with
   | Parsing.Parse_error ->
       (print_string "Erreur de syntaxe\n"; flush stdout; repl lexbuf env))
  
let main = let lexbuf = Lexing.from_channel stdin in repl lexbuf []
  
