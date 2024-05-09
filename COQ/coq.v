(** COQ en COQ 
*)
Require Import String List PeanoNat.
Require Import Decimal Ascii .
Require Import DecimalString .

Open Scope list_scope.
Open Scope string_scope.
Import ListNotations.

(** définition du $\lambda$-terme 
*)
Inductive terme : Set :=
 | C : string -> terme
 | V : string -> terme
 | App : terme -> terme -> terme 
 | Lam :  string -> terme -> terme -> terme .

Definition env : list (string*string) := ("a","1") :: ("b","2") :: ("c","3") :: nil .

Fixpoint mem  (x:string) (l:list string) : bool :=
 match l with
 | nil => false
 | h::t => if h=?x then true else mem x t 
 end.

Fixpoint union (l1 l2: list string) : list string :=
  match l1 with
    | a1::r1 => if mem a1 l2 then union r1 l2
                else  a1 :: (union r1 l2)
    | nil => l2
    end.

Fixpoint remove (var:string) (l: list string) : list string :=
  match l with
   | h::t => if h =? var then remove var t
             else h::(remove var t)
   | nil => nil
  end.

Fixpoint varLibres (lambdaTerm:terme) : list string :=
  match lambdaTerm with
  | C _ => [ ]
  | V x => [ x ]
  | App n m => union (varLibres n) (varLibres m)
  | Lam x tx m => union (remove x (varLibres m)) (remove x (varLibres tx))
  end.


Print DecimalString.

Fixpoint string_of_uint (d:uint) :=
  match d with
  | Nil => EmptyString
  | D0 d => String "0" (string_of_uint d)
  | D1 d => String "1" (string_of_uint d)
  | D2 d => String "2" (string_of_uint d)
  | D3 d => String "3" (string_of_uint d)
  | D4 d => String "4" (string_of_uint d)
  | D5 d => String "5" (string_of_uint d)
  | D6 d => String "6" (string_of_uint d)
  | D7 d => String "7" (string_of_uint d)
  | D8 d => String "8" (string_of_uint d)
  | D9 d => String "9" (string_of_uint d)
  end.


Definition string_of_int (d:int) :=
	match d with
	| Pos d => string_of_uint d
	| Neg d => String "-" (string_of_uint d)
	end.

Fixpoint string_of_nat_aux (time n : nat) (acc : string) : string :=
	let d := match n mod 10 with
		 | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4" | 5 => "5"
		 | 6 => "6" | 7 => "7" | 8 => "8" | _ => "9"
		 end in
	let acc' := d ++ acc in
	match time with
	  | 0 => acc'
	  | S time' =>
	    match n / 10 with
	      | 0 => acc'
	      | n' => string_of_nat_aux time' n' acc'
	    end
	end.

Definition string_of_nat (n : nat) : string :=
	string_of_nat_aux n n "".      

Fixpoint renomme (var:string) (listeVar : list string): string :=
  let fix renommeAux (j fuel:nat) {struct fuel} :string :=  
    let varj := var ++ (string_of_nat j)
    in 
      match fuel with
      | O => var
      | S n => if mem varj listeVar then renommeAux (j + 1) n else varj
      end
  in renommeAux O 10.

Fixpoint substituer (exp: terme) (var: string) (t:terme) {struct exp}:terme :=
  match exp with
  | V x => if x =? var then t else exp
  | C _ => exp
  | App n m => App (substituer n var t) (substituer m var t)
  | Lam x tx m => if negb (mem var (varLibres exp)) then exp
                  else  Lam x (substituer tx var t) (substituer m var t)
    (*  if negb (mem var (varLibres exp)) then exp
      else (* si capture on renomme *)
       if mem x (varLibres t) then
        let newV := renomme x (varLibres t) in
          let newCorps := substituer m x (V newV)
            in Lam newV tx (substituer newCorps var t)
       else Lam x (substituer tx var t) (substituer m var t)
       *)
  end.

Definition estRedex (t:terme) :bool := 
  match t with
  | App (Lam _ _ _ ) _ => true
  | _ => false
  end.

Definition betaReducRedex (redex:terme) :terme :=
 match redex with
 | App (Lam x tx m) n => substituer m x n
 | _ => redex
 end.

Fixpoint reduc (t:terme) :terme :=
  match t with
  | V _ | C _ =>  t
  | Lam x tx m => Lam x tx (reduc m)
  | App n m => 
    if (estRedex t) then betaReducRedex t
    else App (reduc n) m
  end
    .
	  
Fixpoint fullReduc (t:terme) (fuel:nat) {struct fuel}:terme :=
  match fuel with
  | O => t
  | S n => fullReduc (reduc t) n
  end.

Definition t : terme :=  App (Lam "x" (C "type")  (V "x")) (C "tutu").

Print eq.

Compute (fullReduc t 10) = (C "tutu").

Check (O = O).
