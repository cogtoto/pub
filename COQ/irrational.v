(* square root 2 is irrational *)
Require Import Reals.
Require Import Utf8.
Require Import Div2.
Require Import Even.

Coercion INR :nat >-> R.
Coercion IZR : Z >-> R.

Print Nat.div2.

Definition irrational (x : R) : Prop :=
forall (p : Z) (q : nat), q <> 0 -> x <> (p / q)%R.

Theorem double_div2: forall (n : nat), div2 (double n) = n.
Proof.
    intros. 
    induction n.
    - simpl. reflexivity.
    - rewrite double_S.  pattern n at 2. rewrite <-IHn. simpl. reflexivity.

Qed.

Theorem double_inv: forall (n m : nat), double n = double m -> n = m.
Proof.
    intros.
    rewrite <- (double_div2 n) .
    rewrite <- (double_div2 m) .
    rewrite -> H.
    reflexivity.
Qed.

Theorem double_mult_l: forall (n m : nat), double (n * m) = double n * m.
intros. unfold double. ring.
Qed.

Theorem double_mult_r: forall (n m : nat), double (n * m) = n * double m.
unfold double. intros. ring.
Qed.

Theorem even_is_even_times_even: forall (n : nat), even (n * n) -> even n.
intros.

