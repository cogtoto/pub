(* plus *)

Require Import Utf8.

Inductive plusR : nat → nat → nat → Prop :=
| PlusO : ∀ m, plusR O m m
| PlusS : ∀ n m r, plusR n m r
→ plusR (S n) m (S r).

Theorem plus_plusR : forall (n m:nat), plusR n m (plus n m).
Proof.
    intros.
    induction n.
    constructor.

    apply PlusS. simpl.
    exact IHn.
Qed.

Example four_plus_three' : plusR 4 3 7.
apply PlusS. apply PlusS. apply PlusS. apply PlusS. apply PlusO.
Qed.
Print ex_intro.

Example seven_minus_three  : ∃ x, plusR x 3 7.
eapply ex_intro. apply PlusS. apply PlusS. apply PlusS. apply PlusS. apply PlusO.
Qed.

Print seven_minus_three.


Check  (PlusS 3 3 6 (PlusS 2 3 5 (PlusS 1 3 4 (PlusS 0 3 3 (PlusO 3))))).

Print ex.
Print sig.

Check Type->Type.
Check nat->nat.
Check True->False.

Require Extraction.
Extraction Language Scheme.

Definition identité : Type->Type := fun x:Type => x.

Extraction identité.
Print identité.

Definition plus2 : forall _:nat, nat := fun (n:nat) => n+2 .

Print plus2.

Extraction plus2.

Require Import Reals.

Check fun x:R => x.
