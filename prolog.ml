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
  | (a,b)::t -> (imprime a; print_string "<-> "; imprime b;  print_string "\n"; imprime_sigma t ) ;;

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

let sigma1 = [	(Var("x"), Func("h", [Var("b")]));
               (Var("y"),  Func ("f", [Var("x"); Var("y");Var("z");])) ] ;;

let sigma2 = [	(Var("x"), Func("h", [Var("x")]));
               (Var("z"),  Func("g", [Var("x"); Var("a")])) ] ;;

let f1 = Func("f1", [Var("x"); Var("y")]) ;;
let f2 = Func("f2", [Var("z"); Var("u")]) ;;

exception Impossible

let rec listevararg = function
  | [] -> []
  | h::t -> (listevar) h @ (listevararg t)
and listevar = function
  | Var(x) -> [Var(x)]
  | Func(x, t) -> listevararg t

let unifier t1 t2 =
  let rec unificateur t1 t2 =
    match (t1,t2) with
    | (Var(x), _)  -> 
      begin
        if t1 = t2 then [] 
        else if (mem t1 (listevar t2)) then raise Impossible
        else [(t1, t2)]
      end		  
    | (_, Var(x)) -> unificateur t2 t1
    | (Func(x, l1), Func(y, l2)) -> if x<>y then raise Impossible
      else (unifliste l1 l2 [])
  and unifliste l1 l2 sigma =
    match (l1, l2) with
    | ([], _) -> sigma
    | (h1::t1, h2::t2) ->
      begin
        let sigma1 = (unificateur h1 h2) in
        unifliste (map (function terme -> (substituer terme sigma1)) t1)
          (map (function terme -> (substituer terme sigma1)) t2)
          (compose_subst sigma sigma1)
      end		
    | _ -> raise Impossible
  in unificateur t1 t2 

let rec substituer_equ x m equ =
  match equ with
  | (x1,x2)::t -> ((substituer x1 [(x,m)]),(substituer x2 [(x,m)]))::(substituer_equ x m t)
  | [] -> [] 

let rec unifier2 equation =
  match equation with
  | (Func(f1,l1),Func(f2, l2))::equ when f1=f2 -> unifier2 (equ @ combine l1 l2)
  | (x,y)::equ when x=y -> unifier2 equ
  | (Var(x),Func(m,n))::equ when (not (mem (Var(x)) (listevar (Func(m,n))))) 
       -> ((unifier2 (substituer_equ (Var(x)) (Func(m,n)) equ)) @ [(Var(x),Func(m,n))]) 
  | (Func(m,n),Var(x))::equ -> unifier2 ((Var(x), Func(m,n))::equ)
  | (Func(f1,l1),Func(f2, l2))::equ when f1<>f2  -> raise Impossible
  | (Var(x),m)::equ  when (mem (Var(x)) (listevar m)) -> raise Impossible
  | _ -> equation

  let rec unifier3 equation =
    match equation with
    | (Func(f1,l1),Func(f2, l2))::equ when f1=f2 -> unifier3 (equ @ combine l1 l2)
    | (x,y)::equ when x=y -> unifier3 equ
    | (Var(x),Func(m,n))::equ when (not (mem (Var(x)) (listevar (Func(m,n))))) 
         -> begin
            let sigma = (substituer_equ (Var(x)) (Func(m,n)) equ) in
              if sigma = equ then (print_string "if\n"; (unifier3 equ) @ [(Var(x),Func(m,n))])
              else (print_string "else\n" ; unifier3 ( sigma @ [(Var(x),Func(m,n))]) )
            end
    | (Func(m,n),Var(x))::equ -> unifier3 ((Var(x), Func(m,n))::equ)
    | (Func(f1,l1),Func(f2, l2))::equ when f1<>f2  -> raise Impossible
    | (Var(x),m)::equ  when (mem (Var(x)) (listevar m)) -> raise Impossible
    | _ -> equation
  
    let  unifier4 equation =
      let rec aux equa buff =
        match equa with
        | (Func(f1,l1),Func(f2, l2))::equ when f1=f2 -> aux (equ @ combine l1 l2) buff
        | (x,y)::equ when x=y -> aux equ buff
        | (Var(x),Func(m,n))::equ when (not (mem (Var(x)) (listevar (Func(m,n))))) 
            -> begin
                let sigma = (substituer_equ (Var(x)) (Func(m,n)) equ) in
                  if sigma = equ then (print_string "if\n"; (aux equ ((substituer_equ (Var(x)) (Func(m,n)) buff) @ [(Var(x),Func(m,n))])))
                  else (print_string "else\n" ; aux ( sigma @ [(Var(x),Func(m,n))]) (substituer_equ (Var(x)) (Func(m,n)) buff)  )
                end
        | (Func(m,n),Var(x))::equ -> aux ((Var(x), Func(m,n))::equ) buff
        | (Func(f1,l1),Func(f2, l2))::equ when f1<>f2  -> raise Impossible
        | (Var(x),m)::equ  when (mem (Var(x)) (listevar m)) -> raise Impossible
        | _ -> equa @ buff
     in aux equation []


     let  unifier5 equation =
      let rec aux equa buff =
        match equa with
        | (Func(f1,l1),Func(f2, l2))::equ when f1=f2 -> aux (equ @ combine l1 l2) buff
        | (x,y)::equ when x=y -> aux equ buff
        | (Var(x),Func(m,n))::equ when (not (mem (Var(x)) (listevar (Func(m,n))))) 
            -> begin
                let sigma = (substituer_equ (Var(x)) (Func(m,n)) (equ @ buff)) in
                  if sigma = (equ @ buff) then (print_string "if\n"; (aux equ ((substituer_equ (Var(x)) (Func(m,n)) buff) @ [(Var(x),Func(m,n))])))
                  else (print_string "else\n" ; aux ( sigma @ [(Var(x),Func(m,n))]) (substituer_equ (Var(x)) (Func(m,n)) buff)  )
                end
        | (Func(m,n),Var(x))::equ -> aux ((Var(x), Func(m,n))::equ) buff
        | (Func(f1,l1),Func(f2, l2))::equ when f1<>f2  -> raise Impossible
        | (Var(x),m)::equ  when (mem (Var(x)) (listevar m)) -> raise Impossible
        | _ -> equa @ buff
     in aux equation []

let  unifier6 equation =
let rec aux equa buff =
  match equa with
  | (Func(f1,l1),Func(f2, l2))::equ when f1=f2 -> aux (equ @ combine l1 l2) buff
  | (x,y)::equ when x=y -> aux equ buff
  | (Var(x),m)::equ when (not (mem (Var(x)) (listevar m))) 
      -> begin
          let sigma = (substituer_equ (Var(x)) m) (equ @ buff) in
            if sigma = (equ @ buff) then (print_string "if\n"; (aux equ ((substituer_equ (Var(x)) m buff) @ [(Var(x),m)]))) 
            else (print_string "else\n" ; aux ( sigma @ [(Var(x),m)]) (substituer_equ (Var(x)) m buff)  )
          end
  | (Func(m,n),Var(x))::equ -> aux ((Var(x), Func(m,n))::equ) buff
  | (Func(f1,l1),Func(f2, l2))::equ when f1<>f2  -> raise Impossible
  | (Var(x),m)::equ  when (mem (Var(x)) (listevar m)) -> raise Impossible
  | (Var(x),Var(y))::equ -> (print_string "var\n" ; if equ <> [] then aux (equ @ [(Var(x),Var(y))]) buff
                                                     else buff @ [(Var(x),Var(y))])
  | _ -> equa @ buff
in aux equation []

let  unifier7 equation =
  let rec aux equa buff =
    match equa with
    | (Func(f1,l1),Func(f2, l2))::equ when f1=f2 -> aux (equ @ combine l1 l2) buff
    | (x,y)::equ when x=y -> aux equ buff
    | (Var(x),Func(m,n))::equ when (not (mem (Var(x)) (listevar (Func(m,n))))) 
        -> begin
            let sigma = (substituer_equ (Var(x)) (Func(m,n))) (equ @ buff) in
              if sigma = (equ @ buff) then (print_string "if\n"; (aux equ ((substituer_equ (Var(x)) (Func(m,n)) buff) @ [(Var(x),(Func(m,n)))]))) 
              else (print_string "else\n" ; aux ( sigma @ [(Var(x),(Func(m,n)))]) (substituer_equ (Var(x)) (Func(m,n)) buff)  )
            end
    | (Func(m,n),Var(x))::equ -> aux ((Var(x), Func(m,n))::equ) buff
    | (Func(f1,l1),Func(f2, l2))::equ when f1<>f2  -> raise Impossible
    | (Var(x),m)::equ  when (mem (Var(x)) (listevar m)) -> raise Impossible
    | (Var(x),Var(y))::equ -> (print_string "var\n" ; if equ <> [] then aux (equ @ [(Var(x),Var(y))]) buff
                                                       else buff @ [(Var(x),Var(y))])
    | _ -> equa @ buff
  in aux equation []

let rec  unifier8 equation =
  match equation with
  | (Var(x),Var(y)) -> if x=y then [] else [(Var(x), Var(y))] 
  | (Func(f1,l1),Func(f2, l2)) -> if f1 = f2 && List.length l1 = List.length l2 
    then unifierliste (List.combine l1 l2)
    else raise Impossible
  | (Func(m,n),Var(x)) -> unifier8 (Var(x), Func(m,n)) 
  | (Var(x), Func(m,n)) -> if (mem (Var(x)) (listevar (Func(m,n)))) 
    then raise Impossible
    else [(Var(x), Func(m,n)) ] 
and unifierliste = function
  | [] -> []
  | (x,y)::t ->
    let t2 = unifierliste t in
    let t1 = unifier8 ((substituer x t2 ),(substituer y t2)) in
    t1 @ t2

let t1 = Var("x") ;;
let t2 = Func("f2", [Func("a", [Func("b",[])])]) ;;	
let t3 = Func("c", [Func("f2", [Func("a", [Func("b",[])])])])  ;;
let t4 = Func("c", [Var("x")]);;

let equ = (t1,t2)::(t3,t4)::[] ;;

imprime_sigma (unifier3 equ);;

substituer_equ t1 t2 [] ;;

unifier5 [(t1,t2)];;
mem (Var("x")) (listevar (Func("Var(x)", []))) ;;

let t1 = Func("f", [ Func("h", [ Func("a", []); Var("x")])]) 	;;
let t2 = Func("f", [ Var("y") ]) ;;

let t1 = Func("f", [ Func("h", [ Func("a", []); Var("x")]) ; Func("g", [Func("g", [Var("z")])])]) 	;;
let t2 = Func("f", [ Var("y") ; Func("g", [Var("x")]) ]) ;;

imprime_sigma (unifierliste [(t2,t1)] );;

imprime_sigma (unifier t2 t1 );;

let conclusion = hd
let lhypotheses = tl

let rec some f l =
  match l with
  | [] -> false
  | h::t ->  (f h) || (some f t)

let t1 = Func("f", [ Func("h", [ Func("a", []); Var("x")]); Func ("g", [ Func ("g", [Var("z")])])]) ;;

let lregles = [ [ Func("b", []) ] ; [ Func ("c", []) ] ] ;;
let but =  Func("c", []) ;;

let rec affiche_solution lvar lvaleur =
  match (lvar, lvaleur) with
   | ([], []) -> flush stdout 
   | (ah::at, bh::bt) -> (imprime ah; print_string " <-> "; imprime bh; affiche_solution at bt) 
   | _ -> raise Impossible

let c = ref 0
let reset() = c:=0
let next s = incr c; s ^ (string_of_int !c) ;;

let rec removed = function
  | [] -> []
  | h::t -> if (mem h t) then t else h::(removed t) 

let renomme regle =
  let rec lvar_regles = removed ( List.concat (List.map (fun t-> listevar t) regle )) 
  and lvar_newname lvar_r = 
    match lvar_r with
    | [] -> []
    | Var(x)::t -> Var(next "x") :: (lvar_newname t)
    | _ -> raise Impossible
  in sublis (List.combine lvar_regles (lvar_newname lvar_regles)) regle

let question() =
  begin
    print_string "\n autre solution 1/2 (1=oui, 2=non) ? :" ;
    if read_int()= 1  then false else true 
  end

let autre_solution lvar lvaleur  =
  if lvaleur <> [] then (affiche_solution lvar lvaleur ; question())
  else false 

let prolog but lregles =
  let lvar_but = listevar but in
  let rec prouveli lbuts lvaleur =
    match lbuts with
    | [] -> autre_solution lvar_but lvaleur
    | h::t -> 
      some (fun regle ->  try
               let regle_bis = (renomme regle) in
               let sigma1 = unifier h (hd regle_bis) in			
               prouveli 
                 (sublis sigma1 ((lhypotheses regle_bis) @ t))
                 (sublis sigma1 lvaleur)  
             with Impossible -> false)
        lregles 
  in
  prouveli [but] lvar_but

  let prolog2 but lregles =
    let lvar_but = listevar but in
    let rec prouveli lbuts lvaleur =
      match lbuts with
      | [] -> autre_solution lvar_but lvaleur
      | h::t -> 
        some (fun regle ->  try
                 let regle_bis = (renomme regle) in
                 let sigma1 = unifierliste ( [(h,(hd regle_bis))]) in			
                 prouveli 
                   (sublis sigma1 ((lhypotheses regle_bis) @ t))
                   (sublis sigma1 lvaleur)  
               with Impossible -> false)
          lregles 
    in
    prouveli [but] lvar_but



let lregles = [ [ Func("b", [Func("e", []) ])] ; [ Func ("c", []) ] ;  [ Func ("c", [ Var("y") ]) ] ] ;;
let but =  Var("x") ;;

(* les entiers de peano *)
let peano = [ [Func("add", [Var("x"); Func("0", []); Var("x")]) ] ;
              [Func("add", [Var("x");  Func("S", [Var("y")]) ; Func("S", [Var("z")])]) ;
               Func("add", [Var("x"); Var("y"); Var("z")])]
            ] ;;

let but1 = Func("add", [Var("x") ; Var("y") ; Func("S", [Func("S", [Func("S", [Func("0", [])])])])]) ;; 
let but2 = Func("add", [Func("0", []) ; Var("x")  ; Var("x") ]) ;;
(*----*)

prolog2 but1 peano

(* test prolog *)
let t1 = [ [ Func("f", [
    Func("h", [Var("Z")]) ;
    Var("T") ;
    Var("Z") ])] ] ;;

let t2 =   Func("f", [
    Var("X") ;
    Func("g", [Var("X")]) ;
    Var("Y") ])  ;;
(* --- *)

let but =  Var("x") ;;

(* entiers *)
let nat = [ [Func("nat", [Func("0", [])])] ;
            [Func("nat", [Func("s",[Var("X")])]) ; Func("nat", [Var("X")])] ]   ;;

let but = Func("nat", [Var("X")]) ;; 
(* ----- *)

(* genealogie *)
let grecs = [ [Func("mere", [Func("gaia",[]);Func("chronos",[]) ] ) ] ;
              [Func("mere", [Func("rhea",[]);Func("zeus",[]) ] ) ] ;
              [Func("mere", [Func("rhea",[]);Func("hades",[]) ] ) ] ;
              [Func("pere", [Func("zeus",[]);Func("pollux",[]) ] ) ] ;
              [Func("pere", [Func("ourance",[]);Func("chronos",[]) ] ) ] ;
              [Func("pere", [Func("chronos",[]);Func("zeus",[]) ] ) ] ;
              [Func("pere", [Func("zeus",[]);Func("helene",[]) ] ) ] ;
              [Func("pere", [Func("zeux",[]);Func("castor",[]) ] ) ] ;
              [Func("pere", [Func("gaia",[]);Func("chronos",[]) ] ) ] ;
              [Func("parent", [Var("x"); Var("y")]) ; Func("pere", [Var("x"); Var("y")]) ]  ;
              [Func("parent", [Var("x"); Var("y")]) ; Func("mere", [Var("x"); Var("y")]) ]  ;
              [Func("gd-parent", [Var("i"); Var("k")]) ; Func("parent", [Var("i"); Var("j")]) ; Func("parent", [Var("j"); Var("k")])] ;
              [Func("frere", [Var("y"); Var("z")]) ; Func("parent", [Var("x"); Var("y")]) ; Func("parent", [Var("x"); Var("z")])]  							
            ] ;;

let but = Func("gd-parent", [Func("chronos", []) ; Var("x")]) ;;

(* ----- *)
