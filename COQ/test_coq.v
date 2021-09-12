Require Import Nat.
Require Import List.
Require Import Arith.
Require Import Bool.
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

(* append 2 lists of integer *)
Fixpoint append (l1 l2 : list nat) : list nat :=
match l1 with
| nil => l2
| cons x xs  => cons x (append xs l2)
end.

Definition l1 := cons 1 (cons 2 (cons 3 nil)).
Definition l2 := cons 4 (cons 5 (cons 6 nil)).

Compute append l1 l2.


Fixpoint filter_lt (n : nat) (l : list nat) : list nat :=
match l with
| nil => nil
| cons x xs  => if x <? n then cons x (filter_lt  n xs) else filter_lt  n xs
end.

Fixpoint filter_gte (n : nat) (l : list nat) : list nat :=
match l with
| nil => nil
| cons x xs  => if x<=?n then filter_gte n xs else cons x (filter_gte n xs)
end.

Inductive qs_list :list nat -> Type :=
| qs_nil : qs_list nil
| qs_cons : forall (x : nat) (xs : list nat), 
   qs_list (filter_lt x xs) -> qs_list (filter_gte x xs) -> qs_list (cons x xs).
   
   
Fixpoint list_to_qs_list (l : list nat) : qs_list l :=
	match l with
	| nil => qs_nil
	| cons x xs  => qs_cons x xs
	end.






(* does not work as COQ is not aware of the termination *
Fixpoint quicksort (l : list nat) : list nat :=
match l with
| nil => nil
| cons x xs => append (quicksort (filter_lt  x xs)) (cons x (quicksort (filter_gte x xs)))
end.
*)










