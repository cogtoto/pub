(* modus ponens *)
let modus_ponens ha hbc =
  hbc ha

type faux = | ;;

let modus_tollens (hfq:'q->faux) (hpq:'p->'q) (hp:'p) =
  hfq (hpq hp)

;;




  
  