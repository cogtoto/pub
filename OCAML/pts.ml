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
 (* opérateur de point fixe *)
 | Y of terme 
 | Ex of terme * terme (* type existentiel  ex (A:Type) (P: A->Type) *)
 | Ex_intro of terme * terme * terme * terme (* ex_intro (A:Type) (P : A->Type) (x:A) (_: P x) *)
 (* type bool et ifthenelse *)
 | Bool
 | B of bool
 | IfThenElse of terme*terme*terme 
 (* type eq*)
 | Eq of terme*terme*terme 
 | Eq_refl of terme*terme 
  (* type and, le type produit *) 
 | And of terme*terme 
 | Conj of terme*terme 
 | Proj1 of terme
 | Proj2 of terme
 (* type or, le type somme *) 
 | Or of terme*terme
 | Or_introl of terme
 | Or_intror of terme
 | Case of terme*terme*terme
 (* type Nat *)
 | Nat
 | O
 | S of terme
 | Add of terme*terme
 | Sub1 of terme
 | Egal of terme*terme
 (* type True et unique constructeur I, False et fonction d'induction false_ind *)
 | True 
 | I  
 | False
 | False_ind of terme*terme (* application de x:False -> terme *)
 | Type (* le type de Nat, Bool et Type *)

type env = (string * terme) list 
let env0 = [("Type", C "blanc");  ("succ", Prod("x", C "nat", C "nat"))
            ; ("U", C "Type"); ("V", C "Type"); ("u", C "U"); ("v", C "V") ] ;;

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
  | C _ | B _ | Nat | O | False | True | I | Bool | Type -> []
  | App (n, m) | Add (n,m) |  Egal (n, m)   -> union (varLibres n) (varLibres m)
  | Sub1 n -> varLibres n
  | Y m -> varLibres m
  | IfThenElse (b, n, m) -> union (varLibres b) (union (varLibres n) (varLibres m))
  | Lam (x, tx, m) -> union (remove x (varLibres m)) (remove x (varLibres tx))
  | Prod (x, tx, m) -> union (remove x (varLibres m)) (remove x (varLibres tx)) 
  | Eq (t1, t2, t3) -> union (varLibres t1) (union (varLibres t2) (varLibres t3))
  | Eq_refl (t1,t2) -> union (varLibres t1) (varLibres t2)
  | And (t1, t2) ->  union (varLibres t1) (varLibres t2)
  | Conj (t1, t2) ->  union (varLibres t1) (varLibres t2)
  | Proj1 t  -> varLibres t
  | Proj2 t -> varLibres t
  | S t -> varLibres t
  | Or (t1, t2) ->  union (varLibres t1) (varLibres t2)
  | Or_introl t1 -> varLibres t1
  | Or_intror t1 -> varLibres t1
  | Case (t1, t2, t3) -> union (varLibres t1) (union (varLibres t2) (varLibres t3)) 
  | False_ind (t1, t2) ->  union (varLibres t1) (varLibres t2) 
  | Ex (t1,t2) ->  union (varLibres t1) (varLibres t2) 
  | Ex_intro (t1, t2, t3, t4) -> union (varLibres t1) (union (varLibres t2) (union (varLibres t3) (varLibres t4))) 
  
 (** renommer var *)
let renomme var listeVar =
  let rec renommeAux j =
    let varj = var ^ (string_of_int j)
    in if mem varj listeVar then renommeAux (j + 1) else varj
  in renommeAux 0

let rec substituer exp var terme =
  match exp with
  | V x -> if x = var then terme else exp
  | C _ | B _ | Nat | O | True | False | I | Bool|  Type -> exp
  | App (n, m) -> App ((substituer n var terme), (substituer m var terme))
  | Add (n, m) -> Add ((substituer n var terme), (substituer m var terme))
  | Egal (n, m) -> Egal ((substituer n var terme), (substituer m var terme))
  | False_ind (n,m) -> False_ind ((substituer n var terme), (substituer m var terme)) 
  | Sub1 n -> Sub1 (substituer n var terme)
  | Y m -> Y (substituer m var terme) (* faut il substituer dans m ? *)
  | IfThenElse (b, n, m) -> IfThenElse ((substituer b var terme), (substituer n var terme), (substituer m var terme))
  | Eq (t1, t2, t3) -> Eq ((substituer t1 var terme), (substituer t2 var terme),(substituer t3 var terme))
  | Eq_refl (t1, t2) -> Eq_refl ((substituer t1 var terme), (substituer t2 var terme))
  | Ex (t1, t2) -> Ex ((substituer t1 var terme), (substituer t2 var terme))
  | Ex_intro (t1, t2, t3, t4) -> Ex_intro ((substituer t1 var terme), (substituer t2 var terme),(substituer t3 var terme) , (substituer t3 var terme))
  | Or (t1, t2) -> Or ((substituer t1 var terme), (substituer t2 var  terme))
  | Or_introl t1 -> Or_introl (substituer t1 var terme)
  | Or_intror t1 -> Or_intror (substituer t1 var terme)
  | Case (t1, t2, t3) -> Case ((substituer t1 var terme), (substituer t2 var terme),(substituer t3 var terme))
  | And (t1, t2) -> And ((substituer t1 var terme), (substituer t2 var  terme))
  | Conj (t1, t2) -> Conj ((substituer t1 var terme), (substituer t2 var  terme))
  | Proj1 t  -> Proj1 (substituer t var terme)
  | Proj2 t  -> Proj2 (substituer t var terme)
  | S t -> S (substituer t var terme)
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

let estRedex terme =
  match terme with
   | App (Lam _, _) -> true
   | App (Prod _, _) -> true
   | _ -> false

let betaReducRedex redex =
  match redex with
  | App ((Lam (x,tx, m)), n) -> substituer m x n
  | _ -> raise NotRedex

let boolofterme = function
  | B b -> b
  | _ -> raise (Erreur "boolofterme")

let rec somme t1 t2 =
  match t1 with
  | O -> t2
  | S r -> S (somme r t2)
  | _ -> raise (Erreur "somme")

 let rec check terme env =
  match terme with
   | V v -> assoc v env
   | C c -> assoc c env
   | B _ -> Bool 
   | True | False | Bool | Nat -> Type
   | Type -> Type (* risque de boucle ? *)
   | I -> True
   | App (ter1,ter2) -> 
      begin
        let t1  = check ter1 env
         in let t2 = check ter2 env
          in match t1 with
           | Prod(x,ta,tb) when ta = t2 -> substituer tb x ter2 
           | _ -> raise (Erreur "check App" )
      end
    | Lam (v, tv, ter) -> 
        let t1 = check ter ((v,tv)::env)
           in  Prod(v, tv, t1)
    | Prod _ ->  Type 
    | Add (_, _) | Sub1 _ ->  Nat
    | Egal (_, _) -> Bool
    | IfThenElse (_,ter1,_) -> check ter1 env  
    | Y f -> 
       begin 
         match check f env with
          | Prod (_, a, b)  -> a
          | _ -> raise (Erreur "check Y")
       end
    | Eq (t,x,y) -> 
       let tt = check x env in
           Prod ("z", tt, Prod ("w", tt, Type))
    | Eq_refl (t1, t2) ->   Eq(t1,  t2,  t2 )
    | Ex (t1, t2) -> Type
    | Ex_intro (t1,t2,t3,t4) -> Ex(t1, t2)
    | False_ind (t1,t2) ->
       begin
         match (check t2 env) with 
          | False -> t1 
          | _ -> raise (Erreur "check false_ind")
       end
    | Or (_, _) -> Prod ("z", Type, Prod("w", Type, Type))
    | Or_introl t1 -> Or (check t1 env, check t1 env)
    | Or_intror t1 -> Or (check t1 env, check t1 env)
    | Case (t1, t2, t3) ->  
        begin
          match (check t2 env) with 
           | Prod(_, x, y) -> y
           | _ -> raise (Erreur "check case")
        end
    | And (_, _) -> Prod ("z", Type, Prod("w", Type, Type))
    | Conj (t1, t2) -> And (check t1 env, check t2 env)
    | O | S _ -> Nat
    | Proj1 t  ->
      begin 
        match (check t env) with
        | And(t1, _) ->  t1 
        | _ -> raise (Erreur "check Proj1")
      end
    | Proj2 t  ->
      begin 
        match (check t env) with
        | And(_, t2) ->  t2 
        | _ -> raise (Erreur "check Proj2")
      end

 let rec reduc terme =
  match terme with
  | V _ | C _ | Nat | O | True | False | I | Bool | Type -> raise Irreductible
  | B x -> B x
  | Lam (x,tx,m) -> Lam (x,tx,(reduc m))
  | Prod (x,tx,m) -> (try
       Prod (x, tx, (reduc m))
      with Irreductible -> Prod (x, (reduc tx), m)) 
  | App (n, m) ->
    if estRedex terme
    then betaReducRedex terme
    else
      (try App ((reduc n), m)
       with | Irreductible -> App (n, (reduc m)))
  | Add (t1,t2) -> somme (fullReduc t1) (fullReduc t2)
  | Sub1 (S t) -> fullReduc t
  | Egal (t1,t2) -> B ((fullReduc t1) = (fullReduc t2))
  | IfThenElse (b, t1,t2) -> if (boolofterme (fullReduc b)) then  (fullReduc t1) else  (fullReduc t2)
  | Y f -> App(f, (Y f))
  | Eq (t1,t2,t3) -> Eq((fullReduc t1), (fullReduc t2), (fullReduc t3))
  | Eq_refl (t1, t2) -> Eq_refl ((fullReduc t1), (fullReduc t2))
  | Ex (t1, t2) -> Ex ((fullReduc t1), (fullReduc t2))
  | Ex_intro (t1, t2, t3, t4) -> Ex_intro ((fullReduc t1), (fullReduc t2), (fullReduc t3), (fullReduc t4))
  | Or_introl t1 -> Or_introl (fullReduc t1)
  | Or_intror t1 -> Or_intror (fullReduc t1)
  | Case (t1, t2, t3) ->
     begin
      match (fullReduc t1) with
        | Or_introl v -> App(t2, v)
        | Or_intror v -> App(t3, v)
        | _ -> raise (Erreur "reduc case")
     end
  | Or (t1, t2) -> Or ((fullReduc t1), (fullReduc t2))
  | And (t1, t2) -> And ((fullReduc t1), (fullReduc t2))
  | Conj (t1, t2) -> Conj ((fullReduc t1), (fullReduc t2))
  | S t -> S (fullReduc t)
  | Sub1 t -> Sub1 (reduc t)
  | Proj1 t  ->
      begin 
        match (fullReduc t) with
        | Conj(t1, _) -> t1 
        | _ -> raise (Erreur "reduc Proj1")
      end
  | Proj2 t  ->
      begin 
        match (fullReduc t) with
        | Conj(_, t2) -> t2 
        | _ -> raise (Erreur "reduc Proj2")
      end
  | False_ind (t1, t2) -> 
     begin
        match (check t2 env0)  with
         | False -> t1
         |_ -> False_ind (reduc t1, reduc t2)
     end 

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
let rec pr_nat = function
  | O -> 0
  | S t -> 1+ (pr_nat t)
  | _ -> raise (Erreur "pr_nat")

let print terme = 
  let rec aux = function
  | V x | C x -> x
  | B b -> string_of_bool b
  | Lam (x,xt, m) -> lambda ^ x ^ ":" ^ aux xt ^ "." ^ aux m
  | App (n, m) ->  "(" ^ aux n ^ " " ^ aux m ^ ")"
  | Prod (x, xt, False) -> "~" ^ aux xt  
  | Prod (x, xt, m) ->
      if (mem x (varLibres m)) then pi ^ x ^ ":" ^ aux xt ^ "." ^ aux m
      else  "(" ^ aux xt ^ fleche ^ aux m ^ ")"
  | Add (t1, t2) -> aux t1 ^ "+" ^ aux t2 
  | Sub1 t ->  "Sub1 " ^ aux t 
  | Egal (t1, t2) -> aux t1 ^ "=" ^ aux t2 
  | IfThenElse (b, t1, t2) -> " if " ^ aux b ^ " then " ^ aux t1  ^ " else " ^ aux t2
  | Y f -> " Y " ^ aux f
  | Eq (t1, t2, t3) -> "eq(" ^ aux t1 ^ ", " ^ aux t2 ^ ", " ^ aux t3 ^ ")"
  | Eq_refl (t1, t2) -> "eq _refl(" ^ aux t1 ^ ", " ^ aux t2 ^ ")" 
  | Ex (t1, t2) -> "ex(" ^ aux t1 ^ ", " ^ aux t2 ^ ")" 
  | Ex_intro (t1, t2, t3, t4) -> "ex_intro(" ^ aux t1 ^ ", " ^ aux t2 ^ ", " ^ aux t3 ^  aux t4 ^ ")"
  | Or_introl t1 -> "or_introl(" ^ aux t1 ^")"
  | Or_intror t1 -> "or_intror(" ^ aux t1 ^")"
  | Case (t1,t2,t3) -> "case(" ^ aux t1 ^ ", " ^ aux t2 ^ ", " ^ aux t3 ^ ")" 
  | Or (t1, t2) -> aux t1 ^ "\\/" ^ aux t2
  | And (t1, t2) -> aux t1 ^ "/\\" ^ aux t2
  | Conj (t1, t2) -> "conj(" ^ aux t1 ^ "," ^ aux t2  ^")"
  | Proj1 t-> "proj1(" ^ aux t ^") "
  | Proj2 t -> "proj2(" ^ aux t ^") "
  | Nat -> "nat"
  | Bool -> "bool"
  | Type -> "Type"
  | O -> "O"
  | S t -> string_of_int (1 + (pr_nat t))
  | True -> "True"
  | False -> "False"
  | I -> "I"
  | False_ind (t1, t2) -> "false_ind(" ^ aux t1 ^ "," ^ aux t2 ^ ")"
 in print_string (aux terme)


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


(* twice *)
let twice = Lam ("A", C "*", Lam ("f", Prod("z", V "A", V "A"), Lam ("a", V "A", App(V "f", App (V "f", V "a")))))
;;


(* point fixe *)
let multF = 
  Lam ("f", Prod("w",Nat, Nat),
   Lam ("n", Nat, Lam ("m", Nat, 
        IfThenElse(Egal(V "n",O), O, Add (V "m", App(App (V "f", Sub1 (V "n")), V "m"))))))

let mult = Y multF ;;

let rec fois n m = if (n = 0) then 0 else ( (+) m (fois (n-1) m)) ;;

fullReduc (App(App(mult, S (S O)), (S O) ));; 
let facF = Lam("f", Prod ("z", Nat, Nat), 
             Lam ("n", Nat,
              IfThenElse(Egal(V "n", O), S O, (App(App(mult, V "n"), App(V "f", Sub1 (V "n")) ) )))) ;;


let fac = Y facF  ;;

(***************************************************************************************)
let m = Eq_refl(C "nat", S (S O)) ;;
check m env0 ;;

print (check (check m env0) env0) ;;
(* théorèmes *)

let cst2 = Lam("n", Nat, S (S O))  ;;
print (fullReduc (App (cst2, S (S (S (S O))) ))) ;;

let th = Prod("n", Nat, Eq(Nat, App(cst2, V "n"), S (S O) )) 
in print th;;

let proof = Lam("n", Nat, Eq_refl(Nat, App(cst2, V "n"))) in 
  (print proof ; print_newline() ;
   print (check proof env0) ; print_newline() ; 
   print (fullReduc (check proof env0)))  ;;


let proof = Lam("n", Nat, Eq_refl(Nat, App(cst2, V "n"))) in 
 print (check proof env0) ;;

let trivial = Eq(Nat, S (S O), S (S O) ) ;;
print trivial ;;

let htrivial = Eq_refl(Nat, S (S O) ) ;;
print htrivial ;;
print (check htrivial env0 ) ;;

print (check htrivial env0 ) ;;


(************trans*****************)
(*  Theorem trans: forall (A B C:Prop), (A->B) -> (B->C) -> (A->C) . *)
let trans = Prod("A", Type, 
            Prod("B", Type,
            Prod("C", Type,
            Prod("x", Prod("y", V "A", V "B"), Prod("z", Prod("w", V "B", V "C"), 
                                                    Prod("v", V "A", V "C")))))) ;;


(* trans = 
fun (A B C : Prop) (H : A -> B) (H0 : B -> C) (H1 : A) => H0 (H H1)
*)
let proof_trans = Lam("A", Type, 
                  Lam("B", Type,
                  Lam("C", Type,
                  Lam("H", Prod("x", V "A", V "B"),
                  Lam("H0", Prod("x", V "B", V "C"),
                  Lam("H1", V "A",
                   App(V "H0", App (V "H", V "H1")))))))) ;;

print proof_trans ;;

print (check proof_trans env0) ;;
let et p q = Prod("X", Type, Prod ("z", Prod("u", p, Prod("y", q, V "X")), V "X")) ;;

(* type produit *)

let prod_u_v = Lam("X", Type,
       Lam("x", Prod("z", C "U", (Prod ("w", C "V", V "X" ))),App (App (V "x", V "u"), V "v"))) ;;

let trois = S (S (S O));;
let deux = S (S O);;

fullReduc (App(App(mult, trois), deux) );;

print (fullReduc (App(fac, S (S (S (S (S O))))))) ;;

(* Theorem  imp : forall (a b c : Prop), ((a->b) /\ (a->c)) -> a-> (b/\c) *)
let imp = Prod("A", Type, Prod ("B", Type, Prod ("C", Type,
               Prod ("z", And(Prod("x", V "A", V "B"), Prod ("y", V "A", V "C")), 
                     Prod ("w", V "A", And (V "B", V "C"))))))
in print imp ;;


let preuve_imp_ocaml = function h -> (function x -> ((fst h) x, (snd h) x)) ;;
  
let preuve_imp_pts = 
 Lam("A", Type,
   Lam("B", Type,
     Lam("C", Type, 
      Lam("h", And(Prod("x", V "A", V "B"), Prod("y", V "A", V "C")), 
       Lam ("x", V "A", Conj (App(Proj1 (V "h"), V "x"), App(Proj2 (V "h"), V "x"))))))) 
in (print preuve_imp_pts; print_string "\n"; print (check preuve_imp_pts env0))  ;;

let et_refl = 
  Prod("A", Type,
    Prod("B", Type, 
     Prod("x", And(V "A", V "B"), And(V "B", V "A")))) ;;

print et_refl ;;

let preuve_et_refl = 
  Lam("A", Type,
    Lam("B", Type, 
     Lam("h", And(V "A", V "B"), Conj(Proj2 (V "h"), Proj1 (V "h"))))) 
in ( print preuve_et_refl ; print_string "\n"; print(check preuve_et_refl env0)) ;;

let or_elim =
  Prod("A", Type,
    Prod("B", Type,
      Prod("C", Type,
        Prod("x", Prod ("y", V "A", V "B"), Prod ("w", Prod("z", V "B", V "C"), Prod ("v", Or(V "A", V "B"), V "C")))))) ;;

let preuve_or_elim =
  Lam("A", Type,
    Lam("B", Type,
      Lam ("C", Type,
        Lam("h1", Prod("x", V "A", V "C"),
          Lam("h2", Prod("y", V "B", V "C"),
            Lam("h3", Or(V "A", V "B"), 
              Case(V "h3", V "h1", V "h2"))))))) 
  in (print preuve_or_elim ; print_newline() ;
      print (check preuve_or_elim env0)) ;;
  
let non = Lam("P", C "Type",
                Prod("w", V "P", False)) ;;

print (check non env0) ;;
print non;; 
print (reduc (App(non, V "a"))) ;;

let faux_ind = Lam("P", C "Type", 
   Lam ("H", False, False_ind(V "P", V "H"))) ;; 

print (check faux_ind env0) ;;

let impl = Lam("a", Type,
             Lam("b", Type,
               Prod("x", Or(App(non, V "a"), V "b"), Prod("y", V "a", V "b")))) ;;

print (fullReduc impl) ;;
(*implication = 
uA) =>
match H with
| or_introl H1 => False_ind B (H1 H0)
| or_intror H2 => H2
end
	 : forall A B : Prop, ~ A \/ B -> A -> B *)
let preuve_impl = Lam("A", Type,
                    Lam("B", Type,
                       Lam("H", Or(App(non, V "A"), V "B"),
                         Lam("H0", V "A",
                           Case (V "H", 
                                 Lam("x", Prod("w", V "A", False), False_ind(V "B", App(V "x", V "H0"))),
                                 Lam ("y", V "B", V "y")))))) 
in  (print preuve_impl ; print_newline() ;
     print (fullReduc (check preuve_impl env0))) ;;

(* let env0 = [("Type", C "blanc");  ("succ", Prod("x", C "nat", C "nat"))

            ; ("U", C "Type"); ("V", C "Type"); ("u", C "U"); ("v", C "V") ] ;;
*)

let env_classic = [("tiers-exclus",  Or(V "A", App(non, V "A")))] ;;
print (fullReduc (check (C "tiers-exclus") env_classic)) ;;

let th_peirce =
  Prod ("A", Type,
    Prod ("B", Type,
     Prod ("z", Prod("y", Prod("x", V "A", V "B"), V "A"), V "A"))) ;;

 (*    Peirce = 
     fun (A B : Prop) (H : (A -> B) -> A) =>
     let H0 : A \/ ~ A := classic A in
     match H0 with
     | or_introl H1 => H1
     | or_intror H2 => H (fun H1 : A => False_ind B (H2 H1))
     end
  *)

let proof_peirce = 
  Lam("A", Type,
    Lam("B", Type, 
      Lam ("H", Prod("x", Prod("y", V "A", V "B"), V "A"),
        Case(C "tiers-exclus", Lam("zz", V "A", V "zz"),
                               Lam("yy", V "A", App(V "H", Lam("H1", V "A", False_ind(V "B", App(V "yy", V "H1")))))))))
in (print proof_peirce; print_newline();
    print (check proof_peirce env_classic)) ;;
                               
let exf = Lam ("x", False, I) 
in (print exf ; print_newline() ; print (check exf env0)) ;;


 