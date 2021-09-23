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

Theorem length_nplus1: forall (n:nat) (l:int_list), length (cons n l) = 1 + length l.
Proof.
intros n l.
induction l.
simpl. reflexivity.
simpl. reflexivity.
Qed.


Print length_nplus1
.
