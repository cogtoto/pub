open List

type terme = 
  | Var of string
  | Func of string * terme list

exception Impossible


let valeur_subst sigma var =
  try assoc var sigma
  with Not_found -> var

let rec substituer terme sigma  =
  match terme with
  | Var(x) -> (valeur_subst sigma terme)
  | Func(f, []) -> Func(f, []) 
  | Func(f, args) -> Func(f, (map (function t -> (substituer t sigma)) args))
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
;;

let f1 = Func("f", [Var "x"; Func("g", [Var "y"; Var "z"]); Func("h", [Var "x"])]) ;;
let f2 = Func("f", [Func("a",[]); Func("g", [Func("h", [Var "x"]); Func("b", [])]); Func("h", [Func ("a", [])])]) ;;

filtre f1 f2 [] ;;

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

  
let peano = [ (Func("+", [Var "x"; Func("0", [])]), Var "x") ;
              (Func("+", [Var "x"; Func("S", [Var "y"])])), Func("S", [Func("+", [Var "x"; Var "y"])]) ;
              (Func("*", [Var "x"; Func("0", [])]), Func("0", [])) ;
              (Func("*", [Var "x"; Func("S", [Var "y"])])), Func("+", [Var "x"; Func("*", [Var "x"; Var "y"])]) ;
              ] ;;

let un = Func("S", [Func("0", [])]) ;;
let t2 = Func("+", [un; un]) ;;
let x2 = Func("*", [t2;t2]) ;;
let c2 = Func("*", [x2;x2]) ;;

rewrite_bourrin c2 peano ;;    
