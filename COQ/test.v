Section excluded_middle.
Variables A : Prop.

Theorem il_n_est_pas_vrai_que_le_tiers_exclus_est_faux: ~ ~ (~A \/ A).
Proof.
  unfold not.
  intro H.
  apply H.
  left. 
  intro H1.

  apply H.
  right.
  assumption.
Qed.

End excluded_middle.

Locate "<=".
Print ex_intro.

Theorem plus_grand : forall n, exists m, m > n.
Proof.
	intros.
	exists (S n).
	unfold ">".
	unfold "<".
	apply le_n.
Qed.

Print plus_grand.


