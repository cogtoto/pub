Require Import List Bool.

Inductive entier : Set :=
| O : entier
| S : entier -> entier .


Print entier.

Print entier_rect.

Fixpoint somme (a b : entier): entier :=
match a with
| O => b
| S a' => S (somme a' b)
end.

Theorem somme_nulle: forall n:entier, somme n O = n.
Proof.
	intro n. 
	induction n.
	simpl. reflexivity.
	simpl.
	rewrite -> IHn.
	reflexivity.
Qed.

Check somme_nulle.
Print somme_nulle.

Print entier_rec.

Definition toto (n:entier) :entier := n.

Print entier_rec.






