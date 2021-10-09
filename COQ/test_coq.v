Require Import Nat.
Open Scope nat_scope.

Inductive int_list : Set :=
| nil : int_list
| cons : forall (x : nat), forall (xs : int_list), int_list
.


Print int_list.
Print int_list_rec.

Fixpoint length (l: int_list) : nat :=
match l with
| nil => O
| cons _ xs => 1 + length xs
end.

Theorem toujours_vrai: True.
Proof.
exact I.
Qed.

Print toujours_vrai.

Theorem length_nil : length nil = O.
Proof.
    simpl.
    reflexivity.
Qed.

Theorem faux : forall P:Prop, P.
Proof.
Admitted.

Theorem absurdité : 1=2.
Proof.
    exact (faux (1=2)). 
Qed.

Print absurdité.


    


