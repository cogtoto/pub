(* cantor diagonal *)
Require Import Bool.

Section Cantor.

Lemma negb_prop : forall a:bool, negb a =a -> False.
Proof.
    intros.
    unfold negb in H.
    induction a. inversion H. inversion H.
Qed.
     
Definition surjective {X:Type} (f : nat -> X) : Prop := forall y, exists x, f x = y.
Print surjective.


Theorem cantor : ~ exists f : nat -> nat -> bool, surjective f.
Proof.
    intros [f SURJ].
    pose (g := fun b => negb b ). 
 
    (** soit h la diagonalisation négative de la mort *)
    pose (h := fun x => g (f x x)).
    
    (** on applique l'hypothèse de surjection de f sur h *)
    destruct (SURJ h) as [x B].
    assert (C: h x = f x x).
    {
	rewrite B. reflexivity.
    }
    unfold h in C.
    unfold g in C.
    apply negb_prop in C.
    assumption.
Qed.
Print cantor.

End Cantor.
