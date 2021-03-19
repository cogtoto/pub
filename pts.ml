(* Pure Type Systems *)
open List
exception  Erreur of string

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
  | Lam (x, tx, m) -> remove x (varLibres m)
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
  | Lam (x, tx,m) -> (* pas d’occurence libre on en fait rien *)
    if not (mem var (varLibres exp))
    then exp
    else (* si capture on renomme *)
    if mem x (varLibres terme)
    then
      (let newV = renomme x (varLibres terme) in
       let newCorps = substituer m x (V newV)
       in Lam (newV, tx, (substituer newCorps var terme)))
    else Lam (x, tx, (substituer m var terme))
    | _ -> raise (Erreur "substituer")

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
    | _ -> raise (Erreur "typage")
    ;;

 let env0 = [("star", C "blanc"); ("nat", C "star") ; ("O", C "nat") ; ("succ", Prod("x", C "nat", C "nat")) ] ;;

let t1 = App (Lam("n", C "nat", V "n"), C "O") ;;

 typage t1 env0 ;;

