Inductive isZero : nat -> Prop :=
 | IZ : isZero 0.

Theorem iz_zero: isZero 0 .
Proof.
  constructor.
Qed.

Print iz_zero.


Set Printing All.
Check ex.

Theorem  existe0: exists (x:nat), x=O .
Proof.
  exists O.
  reflexivity.
  Qed.

Print existe0.

Check (fun x : nat => @eq nat x O).

Set Printing All.
Print ex_intro.

Set Printing All.
Check (ex_intro (fun x : nat => x =O) O eq_refl).

Print nat.

Theorem arith_f: ~(2+2)=5 .
Proof.
  simpl.
  unfold not.
  intros.
  intuition. 
admit.





Check I.
Check (fun x:False => I).
Check (fun x:True=>False).


Check unit.
Print unit.
Theorem ob : forall x:unit, x=tt .
Proof.
  intros.
  destruct x.
  reflexivity.
 Qed.
 Print ob.

 Print unit_ind.



Axiom classic: forall P: Prop, P\/~P.
Theorem  Peirce: forall A B:Prop, ((A->B)->A)->A.
Proof.
  intros.
  assert (A\/~A) by (apply classic ). 
  destruct H0 as [H1 | H2].
  exact H1.
  apply H .
  intros.
  contradiction.
Qed.

Print Peirce.

Print classic.
Check classic.