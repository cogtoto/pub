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

Fixpoint somme_1_n (n : nat): nat :=
  match n with
  | O => O
  | S n' => n + somme_1_n n'
  end.

Compute somme_1_n 5 .

Require Import Omega.

Theorem th_somme_1_n : forall (n:nat), 2*(somme_1_n n) = n*(n+1).
Proof.
  intros. simpl. 
  induction n.
  assert (som0 : forall (n:nat), somme_1_n n + 0 = somme_1_n n).
  admit.
  rewrite -> som0.
  simpl. reflexivity.
  
  assert (soms0 : forall (n:nat), somme_1_n (S n) + 0 = somme_1_n n).
  admit.

  rewrite -> soms0.
  simpl.
  

  assert (sommen1 : somme_1_n (n+1)= n+1+somme_1_n n).

  admit.
  rewrite -> sommen1.
Qed.

Print th_somme_1_n.