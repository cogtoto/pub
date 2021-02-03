(* leibniz*)
type 'a stream = Cons of 'a * (unit -> 'a stream) ;;
let hd (Cons (h, _)) = h ;;
let tl (Cons (_, tf)) = tf () ;;

let rec take n s =
  if n=0 then []
  else hd s :: take (n-1) (tl s) ;;

let rec sum n s acc =
  if n=0 then acc
  else  sum (n-1) (tl s) (acc +. (hd s)) ;;
  

let rec from i = Cons ((((-1.) ** i ) /. (2.*. i +. 1.)), fun () -> from (i +. 1.)) ;;
let leibniz = from 0. ;;

take 10 leibniz ;;

4. *. sum 5000000 leibniz 0. ;;


(* tirages *)

if Random.bool () then Random.float 1. else (-. Random.float 1.) ;;

let x = if Random.bool () then Random.float 1. else (-. Random.float 1.) ;;
let y = if Random.bool () then Random.float 1. else (-. Random.float 1.) ;;
if (x ** 2. +. y ** 2. <= 1.) then true else false ;;

let gen() = 
  let x = if Random.bool () then Random.float 1. else (-. Random.float 1.) in
  let y = if Random.bool () then Random.float 1. else (-. Random.float 1.) in
  if (x ** 2. +. y ** 2. <= 1.) then 1.0 else 0.0 ;;

let rec from i = Cons (gen(), fun () -> from (i + 1)) ;;

let tirage = from 0 ;;
take 100 tirage ;;
4. *. (sum 5000000  tirage 0. /. 5000000.) ;;

let rec from i = Cons ((4. *. i**2. ) /. (4. *. i**2. -. 1.), fun () -> from (i +. 1.)) ;;
let wallis = from 1. ;;

let rec mult n s acc =
  if n=0 then acc
  else  mult (n-1) (tl s) (acc *. (hd s)) ;;

take 10 wallis ;;

2. *. mult 5000000 wallis 1. ;;

(* calcul de l'intégrale x/(1+x**2) *)
let f x = 1. /. (1. +. x**2.)

let rec somme n i acc =
  if i > n then (1./. n) *. acc 
  else somme n (i +. 1.)  (acc +. (f (i /. n))) ;;

4. *. somme 50000000. 0. 0. ;;

