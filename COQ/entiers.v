Inductive entiers : Set :=
| Zero
| Succ : entiers -> entiers.

Print entiers_ind.

Fixpoint plus (n m:entiers) : entiers :=
match n with
| Zero => m
| Succ n' => Succ (plus n' m)
end.

Theorem th : forall n:entiers, plus n Zero = n.
intros. 
induction n. simpl. reflexivity.
simpl. rewrite IHn. reflexivity.
Qed.

Print th.