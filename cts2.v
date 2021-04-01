Set Printing Implicit.
Set Printing All.

Definition cts2 := fun (n:nat) => 2.

Check cts2.

Print refl_equal.
Print eq_refl.


Theorem th2: forall (n:nat), cts2 n = 2.
Proof.
 intros.
 apply refl_equal.
Qed.
 

Print th2.

Print eq_refl.

Theorem trans: forall (A B C:Prop), (A->B) -> (B->C) -> (A->C) .
Proof.
  intros.
  apply H0.
  apply H.
  trivial.
Qed.

Check @eq_refl nat 2.

Print trans.


Theorem  imp : forall (a b c : Prop), ((a->b) /\ (b->c)) -> a-> (b/\c).
Proof.
  intros.
  destruct H.
  split.
  apply H.
  assumption.
 apply H1. apply H. assumption.
Qed.

Print imp.

Print and.



Require Import Arith List Bool.
Definition factorielle:forall n:nat, nat.
intros.
induction n.
apply 1%nat.
apply Nat.mul.
apply IHn.
apply S.
+ apply n.
Defined.

Print factorielle.

Compute (factorielle 5).
Print nat_rec.
