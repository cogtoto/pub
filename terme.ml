open List

type variable = string
type terme = 
  | Var of string 
  | App of terme * terme 
  | Lam of variable * terme

let rec union l1 l2 =
  if (List.length l1) = 0
  then l2
  else
  if (mem (hd l1) l2) = true
  then union (tl l1) l2
  else (hd l1) :: (union (tl l1) l2)

let rec remove var l =
  if (length l) = 0
  then l
  else
  if var = (hd l) then remove var (tl l) else (hd l) :: (remove var (tl l))

let rec varLibres lambdaTerm =
  match lambdaTerm with
  | Var x -> [ x ]
  | App (n, m) -> union (varLibres n) (varLibres m)
  | Lam (x, m) -> remove x (varLibres m)

let exemple =
  App ((Lam ("x", (App ((Var "y"), (App ((Var "x"), (Var "w"))))))),
       (Lam ("u", (App ((Var "u"), (Var "v"))))))

let exemple2 = Lam ("x", (App ((Var "y"), (App ((Var "x"), (Var "w"))))))

let rec latex_terme terme =
  match terme with
  | Var x -> print_string x
  | Lam (x, m) ->
    (print_string "\\lambda ";
     print_string x;
     print_string " . ";
     latex_terme m)
  | App (n, m) ->
    (print_string "("; latex_terme n; latex_terme m; print_string ")")

let lambda = "\xCE\xBB"

let rec print_terme terme =
  match terme with
  | Var x -> print_string x
  | Lam (x, m) ->
    (print_string lambda;
     print_string x;
     print_string ".";
     print_terme m)
  | App (n, m) -> 
    begin
      match m with 
      | App (_, _) ->  print_terme n;  print_string " (" ; print_terme m; print_string ") "
      | _ -> print_terme n; print_string " "; print_terme m
    end

let rec tikz_terme terme =
  match terme with
  | Var x -> (print_string "node {"; print_string x; print_string " }")
  | Lam (x, m) ->
    (print_string "node {lambda} child { node{";
     print_string x;
     print_string "} } child {";
     tikz_terme m;
     print_string "} ")
  | App (n, m) ->
    (print_string "node {@} child { ";
     tikz_terme n;
     print_string "}  child {";
     tikz_terme m;
     print_string "} ")

let rec caml_terme terme =
  match terme with
  | Var x ->
    (print_string "Var ";
     print_char '"';
     print_string x;
     print_char '"';
     print_string " ")
  | Lam (x, m) ->
    (print_string "Lam(";
     print_char '"';
     print_string x;
     print_char '"';
     print_string ",";
     caml_terme m;
     print_string ")")
  | App (n, m) ->
    (print_string "App(";
     caml_terme n;
     print_string ",";
     caml_terme m;
     print_string ")")

(** renommer var *)
let renomme var listeVar =
  let rec renommeAux j =
    let varj = var ^ (string_of_int j)
    in if mem varj listeVar then renommeAux (j + 1) else varj
  in renommeAux 0

(** substituer terme a var dans exp *)
let rec substituer exp var terme =
  match exp with
  | Var x -> if x = var then terme else exp
  | App (n, m) -> App ((substituer n var terme), (substituer m var terme))
  | Lam (x, m) -> (* pas d’occurence libre on en fait rien *)
    if not (mem var (varLibres exp))
    then exp
    else (* si capture on renomme *)
    if mem x (varLibres terme)
    then
      (let newV = renomme x (varLibres terme) in
       let newCorps = substituer m x (Var newV)
       in Lam (newV, (substituer newCorps var terme)))
    else Lam (x, (substituer m var terme))

let estRedex terme =
  match terme with | App ((Lam (_, _)), _) -> true | _ -> false

exception NOTREDEX

let betaReducRedex redex =
  match redex with
  | App ((Lam (x, m)), n) -> substituer m x n
  | _ -> raise NOTREDEX

exception IRREDUCTIBLE

let rec reduc1Normale terme =
  match terme with
  | Var x -> raise IRREDUCTIBLE
  | Lam (x, m) -> Lam (x, (reduc1Normale m))
  | App (n, m) ->
    if estRedex terme
    then betaReducRedex terme
    else
      (try App ((reduc1Normale n), m)
       with | IRREDUCTIBLE -> App (n, (reduc1Normale m)))

let rec reduc1Valeur terme =
  match terme with
  | Var x -> raise IRREDUCTIBLE
  | Lam (x, m) -> raise IRREDUCTIBLE
  | App (n, m) ->
    (try App (n, (reduc1Valeur m))
     with
     | IRREDUCTIBLE ->
       (try App ((reduc1Valeur n), m)
        with
        | IRREDUCTIBLE ->
          (try betaReducRedex terme
           with | NOTREDEX -> raise IRREDUCTIBLE)))

let rec reduc2Valeur terme = (* réduction "forte" par valeur*)
  match terme with
  | Var x -> raise IRREDUCTIBLE
  | Lam (x, m) -> Lam (x, (reduc2Valeur m))
  | App (n, m) ->
    (try App (n, (reduc1Valeur m))
     with
     | IRREDUCTIBLE ->
       (try App ((reduc1Valeur n), m)
        with
        | IRREDUCTIBLE ->
          (try betaReducRedex terme
           with | NOTREDEX -> raise IRREDUCTIBLE)))

let rec fullReducPrint terme methode =
  try 
    let rec loop terme iter compteur =
      print_string ("[" ^ ((string_of_int compteur) ^ "] -> "));
      print_terme terme; print_string "\n" ;
      let newterme = methode terme
      in
      if (newterme = terme) || (iter = 0)
      then print_newline
      else loop newterme (iter - 1) (compteur + 1)
    in loop terme 50 1
  with IRREDUCTIBLE -> (fun () -> print_string "irréductible\n")

let rec fullReduc terme methode =
  let rec loop terme iter =
    try
      let newterme = methode terme
      in
      if (newterme = terme) || (iter = 0)
      then newterme
      else loop newterme (iter - 1)
    with | IRREDUCTIBLE -> terme
  in loop terme 30000

let betaNormalPrint t = 
  fullReducPrint t reduc1Normale


let betaValeurPrint t = fullReducPrint t reduc1Valeur

let betaValeurFortePrint t = fullReducPrint t reduc2Valeur

let betaValeurForte t = fullReduc t reduc2Valeur

let betaNormal t = fullReduc t reduc1Normale

let betaValeur t = fullReduc t reduc1Valeur

(************SKI***SKI***SKI***SKI**SKI********************)
type ski = 
  | Varia of string
  | I
  | K
  | S
  | Appl of ski*ski 
  | Op of string * ski ;;

exception SkiErreur
exception SkiExec

let rec var  = function
  | Varia x -> [ x ]
  | Appl (n, m) -> union (var n) (var m)
  | Op (x, m) ->  union ([x]) (var m)   
  | I | K | S -> []



let rec lambda_ski = function
  | Lam(x, t) -> lambda_ski_op (Op(x, lambda_ski t))
  | Var(x) -> Varia(x)
  | App(m,n) -> Appl(lambda_ski m, lambda_ski n) 
and lambda_ski_op = function
  | Op(x,Varia y) when x=y -> I 
  | Op(x, t) when not (mem x (var t)) -> Appl(K, t) 
  | Op(x, Appl(m, n))  when (mem x (var m)) || (mem x (var n)) 
    -> Appl(Appl(S, (lambda_ski_op (Op(x,m)))), (lambda_ski_op (Op(x,n)))) 
  | _ -> raise SkiErreur



let rec print_ski  = function
  | Varia x -> print_string x
  | I -> print_string "I" 
  | K -> print_string "K"
  | S -> print_string "S"
  | Appl(m,n) -> print_string "(" ; print_ski m ; print_ski n ; print_string ")"
  | Op(x,m) -> print_string "op" 


let rec ski_lambda = function
  | I -> Lam("x", Var "x")
  | K -> Lam("x", Lam("y", Var "x"))
  | S -> Lam("x", Lam("y", Lam("z", App(App(Var "x", Var "z"), App(Var "y", Var "z")))))
  | Varia(x) -> Var(x)
  | Appl(m,n) -> App(ski_lambda m,ski_lambda n)
  | _ -> raise SkiErreur

let rec exec_aux = function
  | Appl(I, x) -> exec_aux x
  | Appl(Appl(K, x), y) -> exec_aux x
  | Appl(Appl(Appl(S,x),y),z) -> Appl(Appl(exec_aux x, exec_aux z), Appl(exec_aux y,exec_aux z))
  | Appl(x,y) -> Appl(exec_aux x, exec_aux y)
  | Varia x -> Varia x
  | I -> I
  | K -> K
  | S -> S
  | _ -> raise SkiErreur 
and  exec t =
  let r = exec_aux t in
  if r=t then r else exec_aux r ;;

let skk = Appl(Appl(S,K),K) ;;
let sk = Appl(S,K) ;;

exec (Appl(skk, Varia "x")) ;;
let k = Lam("x", Lam("y", Var "x"));;

let s = Lam("x", Lam("y", Lam("z", App(App(Var "x", Var "z"), App(Var "y", Var "z"))))) ;;
let i = Lam("x", Var "x") ;;
let sk = App(s,k);;

betaNormalPrint (App(App(s, App (k, k)), i)) ;;


let rec ski_norm m =
  match m with
  | S | K | I -> m
  | Varia x -> m
  | Appl (m0, m1) ->
    match ski_norm m0 with
    | I -> ski_norm m1
    | Appl (K, m') -> m'
    | Appl (Appl (S, m3), m2) -> ski_norm (Appl (Appl (m3, m1), Appl (m2, m1)))
    | autre -> Appl (autre, ski_norm m1)


open String
open List
(************DE BRUIJN********************)
type tbruijn =
  | Va of int
  | La of tbruijn 
  | Ap of tbruijn * tbruijn

let reste s = int_of_string(sub s 1 ((String.length s)-1)) ;;

let add_env var env =
  (var,0)::map (fun pp -> (fst(pp),(1 + snd(pp)))) env ;;

let t2b terme =
  let l = varLibres terme in
  let rec terme_to_bruijn t env hauteur =
    match t with
    | Var x -> if (mem x l) then Va((reste x) + hauteur) else Va(assoc x env)
    | App (n1, n2) -> Ap (terme_to_bruijn n1 env hauteur, terme_to_bruijn n2 env hauteur) 
    | Lam (x, c) -> La (terme_to_bruijn c (add_env x env) (hauteur+1) )
  in terme_to_bruijn terme [] 0

let decalage d t =
  let rec aux p = function
    | Ap (t1,t2) -> Ap (aux p t1, aux p t2) 
    | La (t) -> La (aux (p+1) t)
    | Va (i) when i<p -> Va(i)
    | Va(i) -> Va (i+d)
  in aux 0 t

let beta_b (La u) t =
  let rec aux p = function
    | Ap (u1,u2) -> Ap (aux p u1, aux p u2)
    | La (v) -> La (aux (p+1) v)
    | Va (i)  when i=p -> decalage p t (*on rend t décalé de la profondeur d'abstr p*)
    | Va (i)  when i<p -> Va (i) (*i est lié, on la rend tel quel *)
    | Va (i) -> Va (i-1) (* on décrèmente la variable libre car la betareduc supprime une lamdda*)
  in aux 0 u ;;

let rec normale_bruijn  = function
  | Va x -> raise IRREDUCTIBLE
  | La n -> La (normale_bruijn n)
  | Ap (La n, m) -> beta_b (La n) m
  | Ap (n,m) -> try Ap (normale_bruijn  n, m)
    with IRREDUCTIBLE -> Ap (n, normale_bruijn  m)

let rec reduc_bruijn t =
  try reduc_bruijn (normale_bruijn t)
  with IRREDUCTIBLE -> t 

let t1 =
  App
    ((Lam ("x", (App ((Lam ("y", (App ((Var "x"), (Var "y"))))), (Var "u1"))))),
     (Var "u2"))
;;

t2b t1 ;;

let zero = Lam ("f", (Lam ("x", Var "x")))
;;
let un = Lam ("f", (Lam ("x", (App ((Var "f"), (Var "x"))))))
;;
let succ =
  Lam ("n",
       (Lam ("f",
             (Lam ("x",
                   (App ((Var "f"), (App ((App ((Var "n"), (Var "f"))), (Var "x"))))))))))
;;
t2b un ;;
reduc_bruijn (Ap ((t2b succ), (t2b zero))) ;;

let t1 = Ap(La(La(Ap (Ap(Va 27,Va 1),Va 0))), La(Ap(Va 25,Va 0))) ;;
