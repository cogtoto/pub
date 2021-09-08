Require Import Nat.
Require Import List.
Open Scope nat_scope.

Theorem modus_ponens : forall (p r : Prop), p -> (p -> r) -> r.
Proof.
	intros p  r Hp Hpr.
	apply Hpr.
	exact Hp.
Qed.

Print modus_ponens.


Theorem modus_tollens : forall (p q :Prop), (p->q) -> not q -> not p.
Proof.
	intros p q Hpq Hnp.
	unfold not in Hnp.
	unfold not.
	intro Hp.
	
	generalize (Hpq Hp).
	exact Hnp.
Qed.

Print modus_tollens.

(* other version of modus_tollens *)
Theorem modus_tollens': forall (p q:Prop), (q->False)-> (p->q) -> (p->False).
Proof.
	intros p q Hfq Hpq Hp.
        generalize (Hpq Hp).
        exact Hfq.
Qed.
Print modus_tollens'.


	
	


