

Definition constantFunc {A B : Type} (b : B) : A -> B :=
  fun _ => b.

Theorem constantFunc_surjective : forall (A B : Type) (b : B), surjective (constantFunc b).
Proof.
  intros A B b.
  unfold surjective.
  intros y.
  exists tt. (* Any arbitrary value in the domain will suffice *)
  reflexivity.
Qed.
