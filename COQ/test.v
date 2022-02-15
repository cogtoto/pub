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

Theorem consequentia_mirabilis : ~~ ((~A -> A) -> A). 
Proof.
  unfold not.
  intros.
  apply H.
  intro H1.
  apply H1.
  intro.
  apply H.
  intro.
  assumption.
Qed.

End excluded_middle.

<<<<<<< HEAD
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


=======
>>>>>>> 525571efcabe636bc42d2c81acd67e4fc7832ea6
