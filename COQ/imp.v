Theorem  imp : forall (a b c : Prop), ((a->b) /\ (a->c)) -> a-> (b/\c).
Proof.
  intros a b c H.
  intro Ha.
  split.
  destruct H as (H1 & H2).
  apply H1. assumption.
  destruct H as (H1 & H2).
  apply H2. assumption.
Qed.

Theorem et_refl: forall (a b:Prop), a/\b -> b/\a .
Proof.
 intros a b H.
 split.
 destruct H as [Ha  Hb].
 assumption.
 destruct H as [Ha Hb].
 assumption.
Qed.

Print et_refl.


Theorem hilbertS : forall (a b c:Prop), (a->b->c)->(a->b)->(a->c).
Proof.
intros a b c.
intros h1 h2 h3.

apply h1.
exact h3.
Show Proof.
apply h2.
exact h3.
Show Proof.
Qed.


Print or.

Theorem or_elim: forall (a b c:Prop), (a->c)->(b->c)->(a\/b)->c.
Proof.
  intros a b c h1 h2 h3.
  destruct h3 as [ha | hb].
  apply h1. exact ha.
  apply h2. exact hb.
Qed.

Print or_elim.

Theorem eq_sym: forall (A:Type) (a b : A), a=b -> b=a .
Proof.
 intros.
 rewrite H.
 reflexivity. 
Qed.

Print eq_sym.

Definition my_True: Prop := forall P:Prop, P-> P.
Print my_True.

Theorem my_I: my_True. 
Proof.
 intros  P p.
 exact p.

Qed.

Print my_I.

Theorem equiv1: my_True -> True.
Proof.
unfold my_True.
intros.
exact I.
Qed.

Print equiv1.

Theorem equiv2: True -> my_True .
Proof.
intros.
unfold my_True.
exact my_I.
Qed.

Inductive binaire :=
| Oui :binaire
| Non :binaire.

Print binaire_ind.

Print my_I.

Inductive even : nat -> Prop :=
| ev_0 : even O 
| ev_SS : forall (n:nat), even n -> even (S (S n)).


Print even_ind.

Theorem quatrepair : even 4.
Proof.
  Show Proof.
    apply ev_SS.
    Show Proof.
      apply ev_SS.
      apply ev_0.
      Show Proof.
      Qed.


Definition add1 : nat->nat. 
Proof.
  intros n.
  Show Proof.
  apply S.

  Show Proof.
  exact n.
  Qed.
  
  Theorem faux : forall P:Prop, P.
  Proof.
  Admitted.

Compute faux (1=2).

Theorem absurdité : 1=2.
Proof.
  exact (faux (1=2)).
Qed.
Print absurdité.:w
:
