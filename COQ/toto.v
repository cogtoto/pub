
Require Import List.
Print list.
Section test.

Variable A:Type.

Inductive ma_liste (T:Type) : Type :=
| Vide : ma_liste T
| Cons : T -> ma_liste  T  -> ma_liste T.

Definition l1 : ma_liste nat :=
    Cons _ 1 (Cons _ 2 (Cons _ 3 (Vide _))).

Check nat.

Definition ide := fun (x:A) => Cons A x (Vide A). 



Print Nat.add.

Lemma add_O : forall n:nat, n = n+0.
intros. 
induction n. simpl. reflexivity.    
simpl. rewrite <- IHn. reflexivity.
Qed.

Lemma add_S : forall (n m:nat), S (m+n) = m+S n.
intros. 
induction m. simpl. reflexivity.
simpl. rewrite <- IHm. reflexivity.
Qed.

Theorem comm : forall (n m:nat), n+m = m+n .
Proof.
intros.
induction n. simpl. rewrite <- add_O. reflexivity.
simpl. rewrite IHn. 
apply add_S. 
Qed.

Require Import Nat.

Theorem yoyo : forall n:nat, (n+1) =? 0 = false.
intro n.
destruct n as [|n'] eqn:E.
simpl. reflexivity.
simpl. reflexivity.
Qed.

Print yoyo.