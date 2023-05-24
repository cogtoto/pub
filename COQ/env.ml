type terme = 
| Lam : (terme -> terme) -> terme
;;

let app t1 t2 = match t1 with
| Lam f -> f t2 ;;

let delta = Lam (fun x -> (app x x))
in app delta delta ;;



type terme =
| Var of string
| Int of int
| Bool of bool
| Vrai
| Faux
| Add of terme * terme
| Sub of terme * terme
| If of terme * terme * terme
| App of terme * terme
| Lam of string * terme
| Fermeture of terme * (terme -> terme)
| Letrec of string * terme * terme
| Egal of terme * terme

let extend t v r = fun x ->  if (t=x) then v else (r x)

let rec eval t r = 
  match t with
   | Var _ -> r t
   | Int _ -> t
   | Vrai -> Vrai
   | Faux -> Faux
   | Bool x -> if x then Vrai else Faux

   | Lam _ -> Fermeture(t,r)
   | Fermeture _ -> t

   | Add ((Int t1), (Int t2)) -> eval (Int (t1+t2)) r
   | Add (t1,t2)  -> eval (Add ((eval t1 r), (eval t2 r))) r

   | Sub ((Int t1), (Int t2)) -> eval (Int (t1-t2)) r
   | Sub (t1,t2)  -> eval (Sub ((eval t1 r), (eval t2 r))) r

   | If (t1, t2, t3) -> if  (Vrai =(eval t1 r )) then (eval t2 r) else (eval t3 r)

   | App (Fermeture (Lam (x,corps),env), t2) -> eval corps (extend (Var x) (eval t2 r ) env) 
   | App (t1,t2)  -> eval (App ((eval t1 r ), (eval t2 r ))) r 
   | Letrec (x1, f1, e) -> eval e (extend_rec (Var x1) f1 r ) 

   | Egal (t1, t2) -> eval (Bool ((eval t1 r ) = (eval t2 r ))) r 

and extend_rec t v r  =
  let rec env_rec  x =
      if (t=x) then eval v (env_rec) else r x
  in env_rec 

;;
let empty_env t = match t with Var x -> Var (x ^  " non définie") | _ -> Var "pas une variable";;

let toto = Lam("a", Var "x") ;;

let m = Letrec ("m", 

               Lam ("a", Lam ("b",
                If ( Egal (Var "b",Int 1), 
                     Var "cucu",
                     Var "coco"))) , 

               App (App (Var "m", Int 2), Int 1)) 
;;

let mult = Letrec ("mult", 

               Lam ("a", Lam ("b",
                If ( Egal (Var "a",Int 1), 
                     Var "b",
                     Add (Var "a", App(App (Var "mult", Sub (Var "a", Int 1)), Var "b"  ))))) , 

               App (App (Var "mult", Int 4), Int 9) )
;;

let popo = App (Lam ("a", Lam ("b", Egal(Var "a", Var "b"))), Int 1976) 
in eval popo empty_env ;;

eval mult empty_env ;;


