(* (a \/ b) -> ((a->b)->b) *)

type ('a, 'b)  union  =
| Un of 'a
| Deux of 'b
;;


let h z f = 
  match z with
  |  (Un x) -> f x
  |  (Deux y) -> y
;;

