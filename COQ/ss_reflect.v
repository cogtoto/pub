From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Set Printing Universes.

Print nat.
Print bool.

Eval compute in 4.+1.
Check prod.
Print prod.

Compute prod nat nat.
Check tt.
Check pair 3 tt.

Definition bar (n : nat) : bool := n == 0.

Check bar.
Compute bar 5.

Definition non_zero n := if n is p.+1 then true else false.

Compute non_zero 3.
Theorem foo : 2 + 2 = 4.
  by []. 
Qed.

Print foo.

Check 1 :: 2 :: 3 :: nil.


Lemma addn0 : forall n, plus n 0 = n.
Proof.
	by elim=> // n IH.
Qed.

