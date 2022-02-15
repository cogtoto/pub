(* coq functional extensionality *)
Require Setoid.

Definition succ_1 n := S n.
Definition succ_2 n := 1 + n.
Definition succ_3 n := n + 1.

Lemma succ_1_eq_succ_2   :  succ_1  = succ_2  .
Proof.
unfold succ_1.
unfold succ_2.
simpl.
reflexivity.
Qed.

Lemma n_plus_1_eq_S1 : forall n, S n = n + 1.
intros.
induction n. reflexivity.
rewrite  -> IHn at 1.
reflexivity.
Qed.

Lemma succ_2_eq_succ_3_fail   :  succ_1  = succ_3  .
unfold succ_1.
unfold succ_3.
simpl. try reflexivity.
Admitted.

Lemma succ_2_eq_succ_3_yes   :  forall n,  succ_1 n  = succ_3 n  .
intros.
unfold succ_1.
unfold succ_3.
apply n_plus_1_eq_S1.
Qed.

Axiom func_ext : forall (A B : Type) (f g : A -> B),
  (forall x, f x = g x) -> f = g.

Lemma succ_2_eq_succ_3 :succ_2 = succ_3.
Proof.  apply func_ext. intros. apply succ_2_eq_succ_3_yes. Qed.


