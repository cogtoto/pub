Require Import Arith.

Fixpoint fact (n:nat):nat :=
match n with 
| O => 1
| S n' => n * fact n'
end.

Compute fact 5.
Print nat.

Inductive factI : nat -> nat ->  Prop :=
| base : factI O 1
| step : forall (n r:nat), factI n r -> factI (S n) ((S n) * r) .

Theorem fact_true : forall (n:nat), factI n (fact n) .
intros.
induction n.
simpl. exact base.
simpl.
apply step.
assumption.
Qed.

