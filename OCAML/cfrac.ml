(* continued fractions *)
open List

let cont a b =
  let rec aux acc a b =
    if a mod b = 0 then
      a::acc
    else  aux ((a / b)::acc) b (a mod b)
  in rev (aux [] a b) ;;

cont 32 7 ;;

(* 
[4;1;1:3]
4+\cfrac{1}{1+\cfrac{1}{1+\cfrac{1}{3}}}
*)

let rec print = function
  | [] -> ""
  | h::[] -> string_of_int h 
  | h::t ->  string_of_int(h) ^ " + \\cfrac{1}{" ^ print t ^ "}" ;;

  
print_string (print (cont 32 7)) ;;
print_string (print (cont 649 200)) ;;


type 'a stream = Cons of 'a * (unit -> 'a stream) ;;

let hd (Cons (h, _)) = h ;;

let tl (Cons (_, tf)) = tf () ;;

let rec take n s =
if n=0 then []
else hd s :: take (n-1) (tl s)

let rec entiers x = Cons(x, fun() -> entiers(x+1))

let rec filtre m (Cons(x,l)) =
if x mod m = 0 then filtre m (l())
else Cons(x, fun() -> (filtre m (l())))

let rec crible (Cons(x,l)) = Cons(x, fun()-> crible(filtre x (l()))) ;;

let rec premiers = crible (entiers 2) ;;

take 1000 premiers  ;;

take 100 (entiers 1) ;;

let rec filtre_jumeaux = function
 | Cons(x,l) -> if (hd (l()) = (x+2)) then Cons(x, fun() -> Cons ((hd (l())) , (fun() ->  (filtre_jumeaux  (l())))))
 else filtre_jumeaux (l()) ;;

take 100 (filtre_jumeaux premiers) ;;

let rec inverse (Cons(x,l)) = Cons(1. /. float_of_int x, fun() -> inverse(l())) ;;

let inverse_jumeaux = inverse (filtre_jumeaux premiers) ;;

let rec somme n s =
if n=0 then 0.
else hd s +. somme (n-1) (tl s) ;;

somme 10000 inverse_jumeaux ;;


