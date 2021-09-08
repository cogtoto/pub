open List

type terme = 
  | Var of string
  | Func of string * terme list

type regle = terme list

let rec substituer_var terme var s =
  match terme with
  | Var(x) -> if x = var then s else Var(x)
  | Func(f, []) -> Func(f, []) 
  | Func(f, args) -> Func(f, (map (function t -> (substituer_var t var s)) args))

let rec imprime_list l = 
  match l with
  | [] -> ()
  | h::t -> (imprime h ; imprime_list t)
and imprime = function
  | Var(x) -> ( print_string " "; print_string x; print_string " " )
  | Func(f, []) -> ( print_string " "; print_string f ; print_string " " )
  | Func (f, args) -> ( print_string "("; print_string f; imprime_list args; print_string ")" )

let rec imprime_sigma = function
  | [] -> ()
  | (a,b)::t -> (imprime a; print_string " -> "; imprime b;  print_string "\n"; imprime_sigma t ) ;;

let valeur_subst sigma var =
  try assoc var sigma
  with Not_found -> var

let domaine_subst sigma =
  concat_map  (function (x,v) -> if x = (valeur_subst sigma x) then [] else [x]) sigma

let rec substituer terme sigma  =
  match terme with
  | Var(x) -> (valeur_subst sigma terme)
  | Func(f, []) -> Func(f, []) 
  | Func(f, args) -> Func(f, (map (function t -> (substituer t sigma)) args))

let rec sublis sigma l =
  match l with
  | [] -> []
  | h::t -> (substituer h sigma) :: (sublis sigma t)

let rec union e1 e2 =
  match e1 with 
  | [] -> e2
  | t::q -> if (mem t e2) then (union q e2) else (t::(union q e2)) 

let compose_subst sigma1 sigma2 =
  map (function var -> (var, (substituer (valeur_subst sigma1 var) sigma2))) 
    (union (domaine_subst sigma1) (domaine_subst sigma2))

  exception Impossible 


let rec listevararg = function
| [] -> []
| h::t -> (listevar) h @ (listevararg t)
and listevar = function
| Var(x) -> [Var(x)]
| Func(x, t) -> listevararg t

  let rec  unifier equation =
    match equation with
    | (Var(x),Var(y)) -> if x=y then [] else [(Var(x), Var(y))] 
    | (Func(f1,l1),Func(f2, l2)) -> if f1 = f2 && List.length l1 = List.length l2 
      then unifierliste (List.combine l1 l2)
      else raise Impossible
    | (Func(m,n),Var(x)) -> unifier (Var(x), Func(m,n)) 
    | (Var(x), Func(m,n)) -> if (mem (Var(x)) (listevar (Func(m,n)))) 
      then raise Impossible
      else [(Var(x), Func(m,n)) ] 
  and unifierliste = function
    | [] -> []
    | (x,y)::t ->
      let t2 = unifierliste t in
      let t1 = unifier ((substituer x t2 ),(substituer y t2)) in
      t1 @ t2
  
let rec filtre_termes lt1 lt2 sigma =
  match (lt1,lt2) with
    | ([], _)  -> sigma
    | (_, []) ->  sigma
    | _ -> 
      begin
    let sigma1 = filtre (hd(lt1)) (hd(lt2)) sigma in
    filtre_termes (tl(lt1)) (tl(lt2)) sigma1
    end 
and filtre m n sigma =
  match (m,n) with
  | (Func(f,_), Func(g, _)) when f <> g -> raise Impossible
  | (Var(x), n) -> 
    begin
    try let var_val = assoc (Var(x)) sigma in
      if var_val = n then sigma else raise Impossible
    with Not_found -> (Var(x), n)::sigma
    end 
  | (Func(f,f1), Func(g,g1)) -> filtre_termes f1 g1 sigma 
  | _ -> raise Impossible

  let rec rewrite t l r =
    match t with
    | Var(_) | Func(_,[]) -> t
    | Func(f, listet)  -> 
      try let subst = filtre l t [] in
        substituer r subst 
      with Impossible -> Func(f, map (function t -> (rewrite t l r)) listet)
    and rewriteall lregles t =
      match lregles with
      | [] -> t
      | (l,r) ::reste -> 
        let t1 = (rewrite t l r) in
          if t1=t then rewriteall reste t 
          else t1
    and rewrite_bourrin t lregles =
      let t1 = rewriteall lregles t in
      if t1=t then t
      else rewrite_bourrin t1 lregles

let rec  unifier_k equation =
  match equation with
  | (Var(x),Var(y)) -> if x=y then [] else [(Var(x), Var(y))] 
  | (Func(f1,l1),Func(f2, l2)) -> if f1 = f2 && List.length l1 = List.length l2 
    then unifierliste_k (List.combine l1 l2)
    else raise Impossible
  | (Func(m,n),Var(x)) -> unifier_k (Var(x), Func(m,n)) 
  | (Var(x), Func(m,n)) -> if (mem (Var(x)) (listevar (Func(m,n)))) 
    then raise Impossible
    else [(Var(x), Func(m,n)) ] 
and unifierliste_k = function
  | [] -> []
  | (x,y)::t ->
    let t2 = unifierliste_k t in
    let t1 = unifier_k ((substituer x t2 ),(substituer y t2)) in
    t1 @ t2

let occurences terme =
let rec occurences_aux i terme  =
  match terme with
  |Var _ | Func(_, []) ->  [i]
  |Func (_, m) ->  [i] @ occur_liste 1 i  m 
and occur_liste  c  i lterme =
  match lterme with
  | [] -> []
  | a::b -> (occurences_aux (int_of_string((string_of_int(i)^(string_of_int c)))) a) @ occur_liste (c+1) i b
in occurences_aux 0 terme
;;

open String

let reste s =
  if (length s) <= 1 then "0"
  else sub s 1 ((length s) -1)
;;


let string_of_char = String.make 1 
;;

let rec cut i terme =
  match terme with
  | Var _ | Func(_, []) when i=0 -> terme
  | Func(f, lt) -> if i=0 then terme 
                   else subterme (int_of_string(string_of_char((string_of_int i).[0]))) 
                                 (int_of_string(reste (string_of_int i)))  (* M/i.u = Mi/u*)
                                 lt
  | _ -> raise Impossible
and subterme i u ltermes =
  match ltermes with
  | hd::tl -> if (i=1) then  cut u hd else  subterme (i-1) u tl
  | [] -> raise Impossible
;;

  let rec greffe i terme greffon =
    match terme with
    | Var _ | Func(_, []) when i=0 -> greffon
    | Func(f, lt) -> if i=0 then greffon 
                     else Func(f, greffeltermes (int_of_string(string_of_char((string_of_int i).[0]))) 
                                   (int_of_string(reste (string_of_int i)))  (* M/i.u = Mi/u*)
                                   lt
                                   greffon)
    | _ -> raise Impossible
  and greffeltermes i u ltermes greffon =
    match ltermes with
    | hd::tl -> if (i=1) then  (greffe u hd greffon)::tl else  hd::(greffeltermes (i-1) u tl greffon)
    | [] -> raise Impossible

 
(** renommer var *)
let renomme var listeVar =
  let rec renommeAux j =
    let varj = var ^ (string_of_int j)
    in if mem varj listeVar then renommeAux (j + 1) else varj
  in renommeAux 0

  let rec listevararg_str = function
  | [] -> []
  | h::t -> (listevar_str) h @ (listevararg_str t)
  and listevar_str = function
  | Var(x) -> [x]
  | Func(x, t) -> listevararg_str t

let rec rename t1 t2 =
  let varliste = listevar_str t2 in
  match t1 with 
  | Var x -> if mem x varliste then Var(renomme x varliste) else Var(x)
  | Func(f, t) -> Func(f, rename_liste t t2)
and rename_liste t t2 =
   match t with 
   | a::b -> (rename a t2)::rename_liste b t2
   | _ -> []   


let rec rename_regle (l1,r1) (l2,r2) =
let t1 = Func("->", [l1;r1]) and
    t2 = Func("->", [l2;r2]) in
    let res =rename t1 t2 in
    match res with
    | Func("->", [g;d]) -> (g,d)
    | _-> raise Exit

let rec remove n = function
 | a::b -> if n=a then b else a::remove n b
 | [] -> []


 exception Impossible_alpha

 let rec filtre_termes_alpha lt1 lt2 sigma =
   match (lt1,lt2) with
   | ([], _) -> sigma
   | (_, []) -> sigma
   | _ ->
   begin
   let sigma1 = filtre_alpha (hd(lt1)) (hd(lt2)) sigma in
   filtre_termes_alpha (tl(lt1)) (tl(lt2)) sigma1
   end
   and filtre_alpha m n sigma =
   match (m,n) with
   | (Var(x), Func(g,_)) -> raise  Impossible_alpha
   | (Func(f, _), Var(x)) -> raise Impossible_alpha
   | (Var x, Var y) -> 
   begin
   try let var_val = assoc (Var(x)) sigma in
   if var_val = Var y then sigma else raise  Impossible_alpha
   with Not_found -> (Var x, Var y)::sigma
   end
   | (Func(f,_), Func(g, _)) when f <> g -> raise  Impossible_alpha
   | (Func(f,f1), Func(g,g1))-> filtre_termes_alpha f1 g1 sigma
 
 let rec alpha_equiv  (l1,r1) (l2,r2) =
   try
    let sigma = filtre_alpha l1 l2 [] in
    (substituer r1 sigma) = (substituer r2 sigma)
   with Impossible_alpha -> false 

let rec alpha_equiv_terme  s t =
try
  let sigma = filtre_alpha s t  [] in
  (substituer s sigma) = (substituer s sigma)
with Impossible_alpha -> false    
   
let superpose l1 l2 = (* rend occurence et la substitution *)
let rec super l1 l2 occ =
  match occ with
  | a::b -> 
    begin
      try 
      let t = cut a l1 in
      match t with 
      | Var _ -> raise Impossible
      | _ -> let sigma = unifier ((cut a l1), l2)
                in (a, sigma)
      with Impossible -> super l1 l2 b 
    end
| [] -> raise Impossible
in if l1=l2 then super l1 (rename l2 l1) (remove 0 (occurences l1)) (* retire 0 car occurence triviale si l1 =l2 *)
            else super l1 (rename l2 l1) (occurences l1) 

let rec super_liste l1 l2 occ =
  match occ with
  | a::b -> 
    begin
      try 
      let t = cut a l1 in
      match t with 
      | Var _ -> raise Impossible
      | _ -> let sigma = unifier ((cut a l1), l2)
                in (a, sigma)
      with Impossible -> super_liste l1 l2 b 
    end
| [] -> raise Impossible

let superpose_liste (l1,r1) (l2,r2) =
let rec superpose_liste_aux l1 l2 occ = (* rend liste des occurences et substitution *)
 if   alpha_equiv (l1,r1) (l2,r2) then 
     try
     let (oc, sigma) = super_liste l1 (rename l2 l1) (remove 0 occ) (* retire 0 car occurence triviale si alpha_equiv *)
        in 
        begin
          print_string "superposition à l'occurence "; print_int oc ;  print_string "\n" ;
          print_string "sur le termes l1 :" ; imprime l1 ; print_string "\n" ;
          print_string "sur le terme l2 :" ; imprime l2 ; print_string "\n" ;
          print_string "avec la substitution :"; imprime_sigma sigma; print_string "\n" ;
        (oc, sigma)::superpose_liste_aux l1 l2 (remove oc occ)
        end
     with Impossible -> []
 else 
    try
    let (oc, sigma) = super_liste l1 (rename l2 l1) occ
      in 
      begin print_string "superposition à l'occurence "; print_int oc ;  print_string "\n" ;
            print_string "sur le terme l1 :" ; imprime l1 ; print_string "\n" ;
            print_string "sur le terme l2 :" ; imprime l2 ; print_string "\n" ;
            print_string "avec la substitution :"; imprime_sigma sigma; print_string "\n" ;
            (oc, sigma)::superpose_liste_aux l1 l2 (remove oc occ)
      end
    with Impossible -> []
in superpose_liste_aux l1 l2 (occurences l1) ;;
 

let cp_all (l1,r1) (l2,r2) = (*rend toutes les paires critiques d'une paire de règles *)
let rec cp_all_aux (l1,r1) (l2,r2) liste =
  match liste with
  |  (oc,sigma)::reste -> (((substituer  r1 sigma), (greffe oc (substituer l1 sigma) (substituer r2 sigma))) )::(cp_all_aux (l1,r1) (l2,r2) reste)
  | [] -> []
in 
let (l'2,r'2) = rename_regle (l2,r2) (l1,r1) in
  cp_all_aux (l1,r1) (l'2,r'2) (superpose_liste (l1,r1) (l'2,r'2)) 


let cpliste rliste (l1,r1) = 
let rec cpliste_aux (l1,r1) rliste acc =
  match rliste with
  | (l2,r2)::reste -> cpliste_aux (l1,r2) reste (cp_all (l1,r1) (l2,r2)::acc)
  | sinon -> acc
in cpliste_aux (l1,r1) rliste []

open List ;;

let rec criticalpairs r =
  let criticalpairs2 r1 r2 =
     concat(map (cpliste r1) r2)
  in criticalpairs2 r r 

let  ordre_inf l1 l2 =  (occurences l1 < occurences l2) 

let  orienter (e1,e2)  =
  if ordre_inf e1 e2 then (e2, e1)
  else (e1,e2) 

let rec orienter_liste = function
 | (e1, e2)::reste -> orienter (e1,e2)::orienter_liste reste
 | [] -> []

let compute_pairs equation = concat(criticalpairs equation)


let question() =
  begin
    print_string "\n itération ? (1=oui, 2=non) " ;
    if read_int()= 2  then false else true 
  end

let simplify (l1,r1) =
  let rec simplify_terme = function
  | Var x -> Var (string_of_char x.[0])
  | Func(f, listet) -> Func(f, simplify_ltermes listet)
  and simplify_ltermes = function
  | a::b -> simplify_terme a :: simplify_ltermes b
  | [] -> []
in (simplify_terme l1, simplify_terme r1)

let normalize (l1,r1) rules =
  let l1_r = rewrite_bourrin l1 rules 
  and r1_r = rewrite_bourrin r1 rules
in (l1_r, r1_r) ;;

let rec normalize_liste l rules =
  match l with
  | (l1,r1)::b -> 
      let l1_r = rewrite_bourrin l1 rules 
      and r1_r = rewrite_bourrin r1 rules
      in
        if (alpha_equiv_terme l1_r r1_r) then  normalize_liste b rules
        else (l1_r, r1_r) ::normalize_liste b rules
  | [] -> []

let rec simplify_equ = function
  | (l1,r1)::b -> if l1=r1 then simplify_equ b else simplify (l1,r1) :: simplify_equ b
  | [] -> [] 

let knuth_complete equation =
  begin
    let r1 = ref equation in
    let r2 = ref equation in
    let cp = ref (compute_pairs equation) in
    while question()
    do
    print_string "démarrage \n" ;
    print_string "\n R1 \n" ;
    imprime_sigma !r1 ;
    r1 := !r2 ;
    r2 := orienter_liste !r2 ;
    imprime_sigma !r2 ;
    print_string "\n R1 orienté \n" ;
    cp := compute_pairs !r2 ;
    print_string "\n new CP \n" ;
    imprime_sigma !cp ;
    cp := orienter_liste !cp ;
    print_string "\n new CP orienté \n" ;
    imprime_sigma !cp ;
    cp := normalize_liste !cp !r2 ;
    print_string "\n new CP orienté et normalisé \n" ;
    imprime_sigma !cp ;
    r2 := !r2@(!cp) ;
    (* r2 := simplify_equ !r2 ; *)
    print_string "\n new R2 \n" ;
    imprime_sigma !r2
    done ;
  end 


(** renommer var *)
let renomme var listeVar =
  let rec renommeAux j =
    let varj = var ^ (string_of_int j)
    in if mem varj listeVar then renommeAux (j + 1) else varj
  in renommeAux 0

open String ;;
   
let a = Func("*", [Func("*", [Var "x"; Var "y"]) ; Func("*", [Var "y"; Var "z"])]) ;;

let a' = rename a a ;;
let b = Var "y" ;;
let b' = Var "y0" ;;

let t1 = Func("*", [Func("*", [Var "x"; Var "y"]) ; Func("*", [])]) ;;
let t2 = Func("*", [Func("*", [Var "x"; Var "y"]) ; Var "z"]) ;;

knuth_complete [(a,b)] ;;

(* 
semi-groupe idem-potent 
(x * y) * z = x * (y * z)
x * x = x
*)
let l1 = Func("*", [Func("*", [Var "x"; Var "y"]); Var "z"]) ;;
let r1 = Func("*", [Var  "x"; Func("*",[Var "y"; Var "z"])]) ;;
let l2 = Func("*", [Var "x"; Var "x"]) ;;
let r2 = Var "x" ;;

 knuth_complete [(l1,r1); (l2,r2)] ;; 