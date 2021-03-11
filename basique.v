Require Import Bool Arith.

Compute if true then 3 else 5.

Definition leibniz (a b:Set):Prop :=
  forall f:Set->Prop, f a -> f b .

Theorem imp: forall (a b c : Prop), ((a->b) /\ (a-> c)) -> a -> (b/\c) .
Proof.
intros.
destruct  H.
split. apply H. assumption.
apply H1. assumption.
Qed.

Print imp.

Inductive entiers : Set := Zero : entiers | S : entiers -> entiers .
Check entiers.
Print entiers.

Print entiers_ind.