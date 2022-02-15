Require Import Utf8.
Require Import Arith.
Require Import Lia.

Print Acc_rect.
Print Acc_inv.

Section popo.
Variable A:Type.
Variable R:A->A->Prop.

Print Acc.


Inductive mon_succ : nat->nat->Prop :=
| Mon_Succ (n:nat) : mon_succ n (S n)
.


Theorem mon_succ_wf : well_founded mon_succ.
unfold well_founded.
intro a.
induction a.
split. intros.
inversion H.

assert (mon_succ a (S a)).
exact (Mon_Succ a).
refine (Acc_intro (S a) _).
intros. inversion H0. assumption.
Qed.

End popo.

Definition div : ∀a b:nat, b<>0 → { q:nat | q*b ≤ a ∧ a < (S q)*b }.
Proof.
intro a; pattern a; apply (well_founded_induction lt_wf); clear a.
intros a Hrec b Hb.

elim (le_lt_dec b a); intros Hab.
assert (H : a-b < a). lia.
elim (Hrec (a-b) H b Hb); simpl; intros q (Hq,Hq').
exists (S q); simpl; lia.
exists 0; lia.
Qed.
