(* cantor diagonal *)
Require Import Bool.

Section Cantor.

Definition surjective {X Y : Type} (f : X -> Y) : Prop := forall y:Y, exists x:X, f x = y.

Theorem cantor X : ~ exists f : X -> X -> Prop, surjective f.
Proof.
    intros [f A].
    pose (g := fun x => ~ f x x).
    destruct (A g) as [x B].
    assert  (C: g x <-> f x x ).
    { 
        rewrite B. tauto.
    }
    unfold g in C.
    tauto.
Qed.

Definition popo := true.
Compute (negb popo).

Lemma negb_prop : forall a:bool, negb a =a -> False.
Proof.
    intros.
    unfold negb in H.
    induction a. inversion H. inversion H.
Qed.
     

Definition surjectiveb {X:Type} (f : nat -> X) : Prop := forall y, exists x, f x = y.

Theorem cantor2 : ~ exists f : nat -> nat -> bool, surjectiveb f.
Proof.
    intros [f A].
    pose (g := fun x => negb (f x x)). 
    destruct (A g) as [x B].
    assert (C: g x =  f x x ).
    { 
        rewrite B. auto.
    }
    unfold g in C. 
    apply  negb_prop in C.
    assumption.
Qed.

End Cantor.