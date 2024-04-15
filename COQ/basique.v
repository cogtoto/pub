Require Import Classical.

Lemma contra : forall (A B:Prop), (not B -> not A) -> (A -> B).
unfold not.
intros A B.
intros p q .

destruct (classic B) as [B_holds | nB_holds].
assumption. 
apply p in nB_holds.
contradiction.
exact q.

Qed.