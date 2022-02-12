(* cantor diagonal *)

open List  ;;
type b = | O | I ;;
let xn = [   [O;I;O;I;O;O;O;I;O;I] ;
             [O;I;O;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
             [O;I;I;I;O;O;O;I;O;I] ;
];;

let f m n =  if (nth (nth xn n) m) = I then I else O ;;

let g = function
| O -> I
| I -> O ;;

let h x = g (f x x);;

 h ;;
