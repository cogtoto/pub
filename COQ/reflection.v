Require Import Nat.

Compute 4/2. 
Compute sqrt 4.

Print nat_ind.


Definition f (x : nat) : nat := 
let fix f_aux (x:nat) (acc:nat) : nat :=
    match acc with
    | 1 => 0
    | S acc'  => if (modulo x acc) =? 0 then 1 else f_aux x acc'
    | 0 => 1
    end
in f_aux x (x-1).

Compute f 7.

Theorem composé_91  : f 91 =  1.
Proof. reflexivity. Qed.

Print composé_91.

Definition g (x:nat) (y:nat) :nat :=
    if  (modulo x y) =? 0 then 1 else 0.

Compute g 91 7.

Theorem composé_91' : exists x : nat, g 91 x = 1.
Proof.
    exists 7.
    reflexivity.
Qed.

Print composé_91'.

Definition  h (x:nat) (y:nat) (z:nat) : nat :=
    if x =? (y*z) then 1 else 0.

Theorem composé_91'' : exists y z : nat, h 91 y z = 1.
Proof.
    exists 7.
    exists 13.
    reflexivity.
Qed.

Print composé_91''.

Fixpoint sum_n (n:nat) : nat :=
    match n with
    | 0 => 0
    | S n' => n + sum_n n'
    end.    


Require Import ZArith.

Require Import ArithRing.

Theorem foo : forall n : nat, 2*n + 1 + n + n = 4*n + 1.
Proof.
    intro.
    ring.
Qed.


Theorem sum_n_terms : forall n : nat, 2 * sum_n n = n*n + n.
Proof.
    intros n. induction n. reflexivity.
    assert (SnSn : S n * S n = n*n + 2*n +1).
    ring.

    rewrite SnSn.
    assert (Sn: S n = n +1).
    ring.
simpl.
ring_simplify.
rewrite IHn.
ring.
Qed.

Inductive tree  :=
 | L : nat -> tree
 | N : tree -> tree -> tree.
 
Check N (L 2) (N (L 3) (L 4)).

Fixpoint sum_tree (t:tree) : nat :=
    match t with
    | L n => n
    | N t1 t2 => sum_tree t1 + sum_tree t2
    end.


Theorem th_sum_tree : forall (t1:tree) (t2:tree), sum_tree (N t1 t2) = sum_tree t1 + sum_tree t2.
Proof.
intros t1 t2.
induction t1. 
simpl.
reflexivity.

simpl.
reflexivity.
Qed.

Inductive even : nat -> Prop :=
  | ev0 : even 0
  | evS : forall (n:nat), odd n -> even (S n)
  with odd : nat -> Prop :=
  | oddS : forall (n:nat), even n -> odd (S n).


Theorem even2k : forall (n:nat), even n
 -> exists (k:nat), n = 2*k.
Proof.
intro n. intro H.

induction n.
exists O.
reflexivity.






Theorem  odd3 : odd 3.
Proof.
exact (oddS 2 (evS 1 (oddS O ev0))).
Qed.

Print odd3.


   


Theorem odd_evenplus1: forall (n:nat), even n -> odd (S n).


Theorem odd2kplus1 : forall (n:nat), not(even n) -> exists (k:nat), n = 2*k + 1.
Proof.
    unfold not.
intros.

destruct  [k Heq].







   
