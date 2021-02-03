(* test *)
(* Hello World! 
*)
type 'a stream = Cons of 'a * (unit -> 'a stream) ;;

let hd (Cons (h, _)) = h ;;
let tl (Cons (_, tf)) = tf () ;;

let inverse_sum n s = 
  let rec aux n s acc =
    if n=0 then acc
    else aux (n-1) (tl s) (acc +. (1. /. (float_of_int (hd s))))
  in aux n s 0. 

let rec take n s =
  if n=0 then []
  else hd s :: take (n-1) (tl s) ;;

let rec entiers x = Cons(x, fun() -> entiers(x+1)) ;;

let rec filtre m (Cons(x,l)) =
  if x mod m = 0 then filtre m (l()) 
  else Cons(x, fun() -> (filtre m (l()))) 

let rec crible (Cons(x,l)) = Cons(x, fun()-> crible(filtre x (l())))

let premiers = crible(entiers 2) ;;

let rec jumeaux (Cons(x, l)) =
  let suivant = hd (l()) 
  in
  if suivant - x =2 then  Cons(x, fun()-> (Cons(suivant, fun()-> (jumeaux (tl(l()))))))
  else jumeaux (l()) ;;

let jum = jumeaux premiers ;;
