(* Pure Type Systems *)
open List
exception Erreur of string
exception NotRedex
exception Irreductible

type terme = 
 | V of string
 | C of string
 | App of terme*terme
 | Lam of string*terme*terme
 | Prod of string*terme*terme
 
type env = (string * terme) list 

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
  | V x -> [ x ]
  | C _ -> []
  | App (n, m) -> union (varLibres n) (varLibres m)
  | Lam (x, tx, m) -> union (remove x (varLibres m)) (remove x (varLibres tx))
  | Prod (x, tx, m) -> remove x (varLibres m)

 (** renommer var *)
let renomme var listeVar =
  let rec renommeAux j =
    let varj = var ^ (string_of_int j)
    in if mem varj listeVar then renommeAux (j + 1) else varj
  in renommeAux 0

let rec substituer exp var terme =
  match exp with
  | V x -> if x = var then terme else exp
  | C _ -> exp
  | App (n, m) -> App ((substituer n var terme), (substituer m var terme))
  | Lam (x, tx, m) -> (* pas d’occurence libre on en fait rien *)
    if not (mem var (varLibres exp)) then exp
      else (* si capture on renomme *)
       if mem x (varLibres terme) then
        (let newV = renomme x (varLibres terme) in
          let newCorps = substituer m x (V newV)
            in Lam (newV, tx, (substituer newCorps var terme)))
       else Lam (x, (substituer tx var terme), (substituer m var terme))
  | Prod (x, tx,m) ->
    if not (mem var (varLibres exp)) (* pas d’occurence libre on en fait rien *)
    then exp
    else if mem x (varLibres terme)   
    then
      (let newV = renomme x (varLibres terme) in
       let newCorps = substituer m x (V newV)
       in Prod (newV, tx, (substituer newCorps var terme)))
    else Prod (x,  (substituer tx var terme), (substituer m var terme))

 let rec typage terme env =
  match terme with
   | V v -> assoc v env
   | C c -> assoc c env
   | App (ter1,ter2) -> 
      begin
        let t1  = typage ter1 env
         in let t2 = typage ter2 env
          in match t1 with
           | Prod(x,ta,tb) when ta = t2 -> substituer tb x ter2 
           | _ -> raise (Erreur "typage App")
      end
    | Lam (v, tv, ter) -> 
        let t1 = typage ter ((v,tv)::env)
           in  Prod(v, tv, t1)
    | Prod _ ->  terme 
    ;;

let estRedex terme =
  match terme with
   | App (Lam _, _) -> true
   | App (Prod _, _) -> true
   | _ -> false

let betaReducRedex redex =
  match redex with
  | App ((Lam (x,tx, m)), n) -> substituer m x n
  | _ -> raise NotRedex

let rec reduc terme =
  match terme with
  | V _ | C _ -> raise Irreductible
  | Lam (x,tx, m) -> Lam (x,tx,  (reduc m))
  | Prod (x, tx, m) -> Prod (x, tx, (reduc m))
  | App (n, m) ->
    if estRedex terme
    then betaReducRedex terme
    else
      (try App ((reduc n), m)
       with | Irreductible -> App (n, (reduc m)))
let rec fullReduc terme =
  let rec loop terme =
    try
      let newterme = reduc terme
      in
      if (newterme = terme)
      then newterme
      else loop newterme
    with | Irreductible -> terme
  in loop terme 
;; 

let lambda = "\xCE\xBB" and pi = "\xCF\x80" and fleche = "\xe2\x86\x92"

let print terme = 
  let rec aux = function
  | V x | C x -> x
  | Lam (x,xt, m) -> lambda ^ x ^ ":" ^ aux xt ^ "." ^ aux m
  | App (n, m) -> 
    begin
      match m with 
      | App (_, _) ->  aux n ^  " (" ^  aux m ^ ") "
      | _ -> aux n ^ " " ^ aux m
    end
  | Prod (x, xt, m) ->
      if (mem x (varLibres m)) then pi ^ x ^ ":" ^ aux xt ^ "." ^ aux m
      else  "(" ^ aux xt ^ fleche ^ aux m ^ ")"
 in print_string (aux terme)


let env0 = [("*", C "blanc"); ("nat", C "*") ; ("O", C "nat") ;  ("1", C "nat"); ("succ", Prod("x", C "nat", C "nat")) ] ;;

let t1 = App (Lam("n", C "nat", V "n"), C "O") ;;
let t2 = Prod("x", C "nat", C "nat") ;;

let t3 = App(C "succ", C "O") ;;
typage t3 env0 ;;

let id = Lam("A", C "*", Lam("x", V "A", V "x")) ;;

let id_nat = App(id, C "nat") ;;

let bool = Prod("X", C "*", (Prod ("X", C "star",  (Prod ("X", C "*", V "X"))))) ;;
let vrai = Lam("X", C "*", Lam ("x", V "X", Lam ("y", V "X", V "x"))) ;;
let faux = Lam("X", C "*", Lam ("x", V "X", Lam ("y", V "X", V "y"))) ;;

(* booléens*)
let app_vrai = App(App(App(vrai, C "nat"), C "1"), C "O") ;;
let app_faux = App(App(App(faux, C "nat"), C "1"), C "O") ;;

typage bool env0 ;;
fullReduc app_vrai ;;

(* Les entiers *)
let entiers = Prod ("X", C "*",  Prod ("x", V "X", Prod ("y", Prod ("z", V "X", V "X"), V "X")))
let zero =  Lam("X", C "*", Lam("x", V "X", Lam ("y", Prod("z", V "X", V "X"), V "x")))

let succ = Lam ("n", entiers, Lam ("X", C "*", Lam ("x", V "X", Lam ("y", Prod("z", V "X", V "X"),
               App(V "y", App (App(App(V "n", V "X"), V "x"), V "y") )))))

let un = App (succ, zero) ;;
let deux = App (succ, un) ;;

let trois = App (succ, deux) ;;

print (fullReduc trois) ;;

(* twice *)
let twice = Lam ("A", C "*", Lam ("f", Prod("z", V "A", V "A"), Lam ("a", V "A", App(V "f", App (V "f", V "a")))))

let plus2  = App(App(twice, entiers), succ) ;;
print (fullReduc (App(plus2, trois))) ;;