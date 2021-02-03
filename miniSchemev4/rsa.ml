open List
open Random 

(* rsa *)  
let p = 61 and q = 53 ;;
let n = p*q ;;
let phi = (p-1)*(q-1) ;; (* phi=3233 *)

let m = 65 ;;

let rec pgcd a b =
  if b = 0 then a 
  else pgcd b ( a mod b)

let rec calcule_e p q =
  let e = Random.int ((p-1)*(q-1))
   in if pgcd e ((p-1)*(q-1)) = 1 then e
     else calcule_e p q  

let rec euclide a b = 
  if b = 0 then ( a , 1 , 0 ) 
   else 
    begin
       let (d', u', v') = euclide b (a mod b)
        in (d', v', u' - (a / b) * v' )
    end

let calcule_d p q e =
   let(_, u ,_) = euclide e (( p-1)*(q-1)) in
     u mod ((p-1)*(q-1))

let rec pow a m = function
  | 0 -> 1 mod m
  | 1 -> a mod m
  | n -> 
    let b = pow a m (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a) mod m
  ;;    


let crypt c e n = pow c n e;;

let e = calcule_e p q  ;;
let d = calcule_d p q e ;;

m ;;
crypt m e n ;;
crypt (crypt m e n) d n;;

(* publique(n,e) = (199387757,16481)
    secrète(n,d) = (199387757,194170193)
*)

let factor n =
  let rec aux n k l =
    if k/2 > n then l
    else if (n mod k) = 0 then aux (n/k) k (k::l)
    else if (k=2) then aux n 3 l 
    else aux n (k+2) l
in rev (aux n 2 [])

