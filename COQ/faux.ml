type faux = | ;;
type vrai = I ;;

let exfalsoquodlibet = fun (f:faux) ->  I;;

(* Inductive and (A B : Prop) : Prop ≜  conj : A ⟶ B ⟶ A ∧ B*)
type ('a,'b) et = Conj of 'a *'b ;;

type ('a,'b) ou = 
  | Or_introl of 'a
  | Or_intror of 'b ;;


let and_comm o = match o with | Conj(o1, o2) -> Conj(o2,o1)

let or_comm o = match o with  
  | Or_introl o1  -> Or_intror o1
  | Or_intror o2  -> Or_introl o2


let modus_tollens (hfq:'q->faux) (hpq:'p->'q) (hp:'p) = 
hfq (hpq hp) ;;