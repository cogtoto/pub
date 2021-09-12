(* PROLOG *)

Inductive entier :=
| O: entier
| S: entier -> entier.

Fixpoint somme (e1 e2:entier):entier :=
  match e1 with
  | O => e2
  | S e1 => S (somme e1 e2)
  end.

Lemma somme_O : forall e:entier, somme e O = e.
Proof.
	intro e.
	induction e.
	simpl. reflexivity.
	simpl. rewrite IHe. reflexivity.
Qed.


Lemma somme_S : forall (e1 e2:entier), somme e1 (S e2)= S (somme e1 e2).
Proof.
	intros.
	induction e1. simpl. reflexivity.
	simpl.
	rewrite IHe1. reflexivity.
Qed.


Compute somme (S O) (S O).

Inductive sommeProlog : entier -> entier -> entier -> Prop :=
| SommeO: forall x, sommeProlog x O x
| SommeS: forall x y z, sommeProlog x y z -> sommeProlog x (S y) (S z).


Example un_plus_un : sommeProlog (S O) (S O) (S (S O)).
Proof.
apply SommeS. apply SommeO.
Defined.

Theorem somme_deux_entiers : forall e1 e2, sommeProlog e1 e2 (somme e1 e2).
Proof.
	intros e1 e2.
	induction e2. 
	rewrite somme_O.
	apply SommeO.
	rewrite somme_S.
	apply SommeS.
	exact IHe2.
Qed.

Lemma somme_prolog_o : forall e1, sommeProlog e1 O e1.
Proof.
	intro e1.
	induction e1.
	apply SommeO.
	apply SommeS.


Theorem somme_deux_entiers' : forall e1 e2 e3, sommeProlog e1 e2 e3 -> e3 = somme e1 e2 .
intros.
induction e2.
rewrite somme_O.



	