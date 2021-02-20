(* inference de type *)

type ltype = 
  | Int 
  | Vart of string
  | Fleche of ltype*ltype

type env = ltype list

let imprime ty = 
  let rec aux = function
   | Int  -> "entier"
   | Vart v -> v
   | Fleche (t1,t2) -> aux t1 ^ " -> " ^ aux t2
in aux ty ;;

let rec imprime_env = function
  | [] -> ""
  | a::b -> (imprime a) ^  " ; " ^ (imprime_env b) ;;


(* preliminary step / création variables de types*)
type terme = 
  | Var of string 
  | App of terme * terme 
  | Lam of string * terme
  | Const of int
  | Plus of terme * terme

open String
let occurences terme =
  let rec aux  occ terme =
  match terme with
  |Var _ | Const _ -> [occ]
  |Lam (v, m) ->  [occ] @ 
                  [int_of_string((string_of_int(occ)^(string_of_int 1)))] @ 
                   aux (int_of_string((string_of_int(occ)^(string_of_int 2))))  m
  |App (n,m) | Plus (n,m) -> [occ] @
                aux (int_of_string((string_of_int(occ)^(string_of_int 1))))  n @
                aux (int_of_string((string_of_int(occ)^(string_of_int 2))))  m
    in aux 0  terme

let string_of_char = String.make 1 ;;

let reste s =
  if (length s) <= 1 then "0"
  else sub s 1 ((length s) -1)

exception Erreur of string

let rec cut occ terme =
  match terme with
  | _ when occ=0 -> terme
  |   Lam(v,m) when (int_of_string(string_of_char((string_of_int occ).[0]))) = 1 -> Var v
  |   Lam(v,m) when (int_of_string(string_of_char((string_of_int occ).[0]))) = 2  
                    ->  cut (int_of_string(reste (string_of_int occ)))  m
  |   App(n,m) | Plus(n,m) when (int_of_string(string_of_char((string_of_int occ).[0]))) = 1 
                            -> cut (int_of_string(reste (string_of_int occ))) n
  |   App(n,m) | Plus(n,m) when (int_of_string(string_of_char((string_of_int occ).[0]))) = 2 
                            -> cut (int_of_string(reste (string_of_int occ))) m
  |   _ -> raise (Erreur "cut") 
    
 (* parcours rend une liste [ (occurence,variable); ... ] *)
let  parcours terme =
    let rec aux l i =
     match l with 
     | [] -> []
     | a::b -> 
       begin
        match (cut a terme) with
         | Var v -> (a, "alpha_" ^v ) :: aux b i
         | Const _ -> (a, "alpha_entier") ::  aux b i
         | Lam _ | App _| Plus _ -> (a, "alpha_" ^ string_of_int i) :: aux b (i+1) 
       end
  in aux (occurences terme) 1
  
let rec hm terme =
  let p = parcours terme in
  let rec aux l =
    match l with
    | [] -> []
    | (oc, ty)::b -> 
       begin
        match (cut oc terme) with
         | Const _  -> (Vart ty, Int) :: aux b
         | Var _ -> (Vart ty, Vart ty) :: aux b
         | Plus (e1, e2) -> 
              let e1_type = List.assoc (int_of_string (string_of_int oc ^ "1")) p 
              and e2_type =  List.assoc (int_of_string (string_of_int oc ^ "2")) p
                in (Vart ty, Int) :: (Vart e1_type, Int) :: (Vart e2_type, Int) :: aux b
         | App (e1,e2) -> 
              let e1_type = List.assoc (int_of_string (string_of_int oc ^ "1")) p 
              and e2_type =  List.assoc (int_of_string (string_of_int oc ^ "2")) p
                in  (Vart e1_type, Fleche(Vart e2_type , Vart ty) ) :: aux b 
         | Lam (v,e) -> 
              let v_type = List.assoc (int_of_string (string_of_int oc ^ "1")) p 
              and e_type =  List.assoc (int_of_string (string_of_int oc ^ "2")) p
                in  (Vart ty, Fleche(Vart v_type , Vart e_type) ) :: aux b 
       end 
  in aux p

let valeur_subst sigma var =
    try List.assoc var sigma
    with Not_found -> var

let rec substituer t sigma =
match t with
| Int  -> Int
| Vart(x) -> (valeur_subst sigma t)
| Fleche(t1, t2) -> Fleche(substituer t1 sigma,substituer t2 sigma ) 

open List 

let rec union l1 l2 =
  if (List.length l1) = 0
  then l2
  else
  if (mem (hd l1) l2) = true
  then union (tl l1) l2
  else (hd l1) :: (union (tl l1) l2)

let rec listevararg = function
  | [] -> []
  | h::t -> (listevar h) @ (listevararg t)
and listevar = function
  | Vart(x) -> [Vart(x)]
  | Fleche(m, n) -> union (listevar m) (listevar n) 
  | Int -> [] ;;

  let rec unifier equation =
    match equation with
    | (Int, Int) -> []
    | (Vart(x),Vart(y)) -> if x=y then [] else [(Vart(x), Vart(y))]
    | (Fleche(f1,l1),Fleche(f2, l2)) ->  unifierliste [(f1, f2)] @ [(l1, l2)]
    | (Fleche(m,n),Vart(x)) -> unifier (Vart(x), Fleche(m,n))
    | (Vart(x), Fleche(m,n)) -> if (List.mem (Vart(x)) (listevar (Fleche(m,n))))
                               then raise (Erreur "unification impossible")
                              else [(Vart(x), Fleche(m,n)) ]
    | (Vart(x), Int) -> [(Vart(x), Int) ]
    | (Int, Vart(x)) -> [(Vart(x), Int) ]
    | (e1,e2) -> raise (Erreur ( "unif impossible \n" ^ (imprime e1) ^ " = " ^ (imprime e2) ) )
     and unifierliste = function
    | [] -> []
    | (x,y)::t ->
    let t2 = unifierliste t in
    let t1 = unifier ((substituer x t2 ),(substituer y t2)) in
    t1 @ t2

  let t1 =
    App
      ((Lam ("x", (App ((Lam ("y", (App ((Var "x"), (Var "y"))))), (Var "u"))))),
      (Var "z")) ;;

  let t2 = Lam ("x", Var "x") ;;

let t3 = Lam ("f", Lam ("x", App(Var "f", Var "x"))) ;;
let t4 =  Lam ("x", Plus(Var "x", Const 5)) ;;

let infer t = 
  let p = parcours t in
  let c = hm t in
  substituer (Vart (assoc 0 p)) (unifierliste c) ;;
