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
 | N of int
 | B of bool
 | Add of terme*terme
 | Sub of terme*terme
 | Egal of terme*terme
 | IfThenElse of terme*terme*terme 
 | Y of terme 
 | Eq of terme*terme*terme
 | Eq_refl of terme*terme 

 
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
  | C _ | N _ | B _ -> []
  | App (n, m) | Add (n,m) | Sub (n, m) | Egal (n, m)   -> union (varLibres n) (varLibres m)
  | Y m -> varLibres m
  | IfThenElse (b, n, m) -> union (varLibres b) (union (varLibres n) (varLibres m))
  | Lam (x, tx, m) -> union (remove x (varLibres m)) (remove x (varLibres tx))
  | Prod (x, tx, m) -> union (remove x (varLibres m)) (remove x (varLibres tx)) 
  | Eq (t1, t2, t3) -> union (varLibres t1) (union (varLibres t2) (varLibres t3))
  | Eq_refl (t1,t2) -> union (varLibres t1) (varLibres t2)

 (** renommer var *)
let renomme var listeVar =
  let rec renommeAux j =
    let varj = var ^ (string_of_int j)
    in if mem varj listeVar then renommeAux (j + 1) else varj
  in renommeAux 0

let rec substituer exp var terme =
  match exp with
  | V x -> if x = var then terme else exp
  | C _ | N _ | B _ -> exp
  | App (n, m) -> App ((substituer n var terme), (substituer m var terme))
  | Add (n, m) -> Add ((substituer n var terme), (substituer m var terme))
  | Egal (n, m) -> Egal ((substituer n var terme), (substituer m var terme))
  | Sub (n, m) -> Sub ((substituer n var terme), (substituer m var terme))
  | Y m -> Y (substituer m var terme) (* faut il substituer dans m ? *)
  | IfThenElse (b, n, m) -> IfThenElse ((substituer b var terme), (substituer n var terme), (substituer m var terme))
  | Eq (t1, t2, t3) -> Eq ((substituer t1 var terme), (substituer t2 var terme),(substituer t3 var terme))
  | Eq_refl (t1, t2) -> Eq_refl ((substituer t1 var terme), (substituer t2 var terme))
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

 let rec check terme env =
  match terme with
   | V v -> assoc v env
   | C c -> assoc c env
   | N _ -> C "nat"
   | B _ -> C "booleen"
   | App (ter1,ter2) -> 
      begin
        let t1  = check ter1 env
         in let t2 = check ter2 env
          in match t1 with
           | Prod(x,ta,tb) when ta = t2 -> substituer tb x ter2 
           | _ -> raise (Erreur "check App")
      end
    | Lam (v, tv, ter) -> 
        let t1 = check ter ((v,tv)::env)
           in  Prod(v, tv, t1)
    | Prod _ ->  C "Type" 
    | Add (_, _) | Sub (_,_) -> C "nat"
    | Egal (_, _) -> C "booleen"
    | IfThenElse (_,ter1,_) -> check ter1 env  
    | Y f -> 
       begin 
         match check f env with
          | Prod (_, a, b)  -> a
          | _ -> raise (Erreur "check Y")
       end
    | Eq (t,x,y) -> 
       let tt = check x env in
           Prod ("z", tt, Prod ("w", tt, C "Type"))
    | Eq_refl (t1, t2) ->   Eq(t1,  t2,  t2 )

let estRedex terme =
  match terme with
   | App (Lam _, _) -> true
   | App (Prod _, _) -> true
   | _ -> false

let betaReducRedex redex =
  match redex with
  | App ((Lam (x,tx, m)), n) -> substituer m x n
  | _ -> raise NotRedex

let intofterme = function
  | N n -> n
  | _ -> raise (Erreur "intofterme")

let boolofterme = function
  | B b -> b
  | _ -> raise (Erreur "boolofterme")

let rec reduc terme =
  match terme with
  | V _ | C _ -> raise Irreductible
  | N x  -> N x 
  | B x -> B x
  | Lam (x,tx,m) -> Lam (x,tx,  (reduc m))
  | Prod (x, tx,m) -> Prod (x, tx, (reduc m))
  | App (n, m) ->
    if estRedex terme
    then betaReducRedex terme
    else
      (try App ((reduc n), m)
       with | Irreductible -> App (n, (reduc m)))
  | Add (t1,t2) -> N ((intofterme (fullReduc t1)) + (intofterme (fullReduc t2)))
  | Sub (t1,t2) -> N ((intofterme (fullReduc t1)) - (intofterme (fullReduc t2)))
  | Egal (t1,t2) -> B ((intofterme (fullReduc t1)) = (intofterme (fullReduc t2)))
  | IfThenElse (b, t1,t2) -> if (boolofterme (fullReduc b)) then  (reduc t1) else  (reduc t2)
  | Y f -> App(f, (Y f))
  | Eq (t1,t2,t3) -> Eq((fullReduc t1), (fullReduc t2), (fullReduc t3))
  | Eq_refl (t1, t2) -> Eq_refl ((fullReduc t1), (fullReduc t2))
 and 
fullReduc terme =
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
  | N n -> string_of_int n
  | B b -> string_of_bool b
  | Lam (x,xt, m) -> lambda ^ x ^ ":" ^ aux xt ^ "." ^ aux m
  | App (n, m) ->  " ( " ^ aux n ^ " " ^ aux m ^ ")"
  | Prod (x, xt, m) ->
      if (mem x (varLibres m)) then pi ^ x ^ ":" ^ aux xt ^ "." ^ aux m
      else  "(" ^ aux xt ^ fleche ^ aux m ^ ")"
  | Add (t1, t2) -> aux t1 ^ "+" ^ aux t2 
  | Sub (t1, t2) -> aux t1 ^ "-" ^ aux t2 
  | Egal (t1, t2) -> aux t1 ^ "=" ^ aux t2 
  | IfThenElse (b, t1, t2) -> " if " ^ aux b ^ " then " ^ aux t1  ^ " else " ^ aux t2
  | Y f -> " Y " ^ aux f
  | Eq (t1, t2, t3) -> " Eq(" ^ aux t1 ^ ", " ^ aux t2 ^ ", " ^ aux t3 ^ ") "
  | Eq_refl (t1, t2) -> " Eq _refl(" ^ aux t1 ^ ", " ^ aux t2 ^ ") " 
 in print_string (aux terme)


let env0 = [("Type", C "blanc"); ("nat", C "Type") ; ("O", C "nat") ;  ("1", C "nat"); ("succ", Prod("x", C "nat", C "nat"))
            ; ("U", C "nat"); ("V", C "nat"); ("u", C "U"); ("v", C "V") ] ;;

let t1 = App (Lam("n", C "nat", V "n"), C "O") ;;
let t2 = Prod("x", C "nat", C "nat") ;;

let t3 = App(C "succ", C "O") ;;
check t3 env0 ;;

let id = Lam("A", C "*", Lam("x", V "A", V "x")) ;;

let id_nat = App(id, C "nat") ;;

let bool = Prod("X", C "*", (Prod ("X", C "star",  (Prod ("X", C "*", V "X"))))) ;;
let vrai = Lam("X", C "*", Lam ("x", V "X", Lam ("y", V "X", V "x"))) ;;
let faux = Lam("X", C "*", Lam ("x", V "X", Lam ("y", V "X", V "y"))) ;;

(* booléens*)
let app_vrai = App(App(App(vrai, C "nat"), C "1"), C "O") ;;
let app_faux = App(App(App(faux, C "nat"), C "1"), C "O") ;;

check bool env0 ;;
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

let popo = IfThenElse (Egal(N 7, Add (N 2, Sub (N 10, N 5))), N 100, N 999) in
   ( print (fullReduc popo) ; print_string "\n"; check popo env0) ;;

let pipi = IfThenElse (Egal(N 7, Add (N 2, Sub (N 10, N 5))), N 100, N 999) in
   ( print pipi ; print_string "\n"; check pipi env0) ;;

let add100 = App(Lam("n", C "nat", Add (V "n", N 100)), N 2) in print (fullReduc add100) ;;

let pnul = Prod("n", C "nat", Prod("z", Egal(V "n", N 0), C "*" )) ;;

let nullify = Lam("x", C "nat", N 0) ;;

(* type produit *)

let env0 = [("*", C "blanc"); ("nat", C "*") ; ("O", C "nat") ;  ("1", C "nat"); ("succ", Prod("x", C "nat", C "nat"))
            ; ("U", C "nat"); ("V", C "nat"); ("u", C "U"); ("v", C "V") ] ;;


let prod_100_101 = Lam("X", C "nat", Lam("x", Prod("z", C "nat", (Prod ("w", C "nat", V "X" ))),App (App (V "x", N 100), N 101))) ;;
print (check prod_100_101 env0) ;;

let proj1 = Lam("t", (check prod_100_101 env0), App(App(V "t", V "U"), Lam ("x", V "U", Lam ("y", V "V", V "x")) ))
in print (fullReduc (App (proj1, prod_100_101)))
;

let proj2 = Lam("t", (check prod_100_101 env0), App(App(V "t", V "U"), Lam ("x", V "U", Lam ("y", V "V", V "y")) ))
in print (fullReduc (App (proj2, prod_100_101)))
;;


(* point fixe *)
let multF = 
  Lam ("f", Prod("w", C "nat", C "nat"),
   Lam ("n", C "nat", Lam ("m", C "nat", 
        IfThenElse(Egal(V "n", N 0), N 0, Add (V "m", App(App (V "f", Sub (V "n", N 1)), V "m"))))))

let mult = Y multF ;;

let facF = Lam("f", Prod ("z", C "nat", C "nat"), 
             Lam ("n", C "nat",
              IfThenElse(Egal(V "n", N 0), N 1, (App(App(mult, V "n"), App(V "f", Sub (V "n", N 1)) ) )))) ;;

let fac = Y facF  ;;

fullReduc (App(fac, N 5)) ;;
fullReduc (App(App(mult, N 5), N 6)) ;;
(***************************************************************************************)
let m = Eq_refl(C "nat", N 2) ;;
check m env0 ;;

print (check (check m env0) env0) ;;
(* théorèmes *)

let cst2 = Lam("n", C "nat", N 2)  ;;
print (fullReduc (App (cst2, N 10))) ;;

let th = Prod("n", C "nat", Eq(C "nat", App(cst2, V "n"), N 2)) 
in print th;;
let proof = Lam("n", C "nat", Eq_refl(C "nat", App(cst2, V "n"))) in 
 print proof  ;;
let proof = Lam("n", C "nat", Eq_refl(C "nat", App(cst2, V "n"))) in 
 print (check proof env0) ;;


let trivial = Eq(C "nat", N 2, N 2) ;;
print trivial ;;

let htrivial = Eq_refl(C "nat", N 2) ;;
print htrivial ;;
print (check htrivial env0 ) ;;


let faux = Eq(C "nat", N 2, N 1) ;;
print faux ;;

let h = Eq_refl(C "nat", N 2) ;;
print htrivial ;;
print (check htrivial env0 ) ;;


(************trans*****************)
(*  Theorem trans: forall (A B C:Prop), (A->B) -> (B->C) -> (A->C) . *)
let trans = Prod("A", C "Type", 
            Prod("B", C "Type",
            Prod("C", C "Type",
            Prod("x", Prod("y", V "A", V "B"), Prod("z", Prod("w", V "B", V "C"), 
                                                    Prod("v", V "A", V "C")))))) ;;


(* trans = 
fun (A B C : Prop) (H : A -> B) (H0 : B -> C) (H1 : A) => H0 (H H1)
*)
let proof_trans = Lam("A", C "Type", 
                  Lam("B", C "Type",
                  Lam("C", C "Type",
                  Lam("H", Prod("x", V "A", V "B"),
                  Lam("H0", Prod("x", V "B", V "C"),
                  Lam("H1", V "A",
                   App(V "H0", App (V "H", V "H1")))))))) ;;

print proof_trans ;;

check proof_trans env0 ;;

(****************ET ET ET ET *******)
let et p q = Prod("X", C "Type", Prod ("z", Prod("u", p, Prod("y", q, V "X")), V "X")) ;;

(*
and A B, written A /\ B, is the conjunction of A and B
conj p q is a proof of A /\ B as soon as p is a proof of A and q a proof of B
proj1 and proj2 are first and second projections of a conjunction 
*)