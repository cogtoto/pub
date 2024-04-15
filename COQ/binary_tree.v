Require Import Arith.

Inductive bin : Type :=
| L : bin
| N : bin -> bin -> bin .

Definition t1 : bin := N (N L L) L .

Fixpoint size (t:bin) : nat :=
    match t with
    | L => 1
    | N t1 t2 => 1 + size t1 + size t2
    end.

Compute size t1.


Theorem size0: forall t:bin, size t <> O .
Proof.
    intros.
    unfold not.
    unfold size. simpl. case t.
    discriminate.
    discriminate.
    Qed.

Fixpoint append (t1 t2:bin) : bin :=
    match t1 with
    | L => N L t2
    | N g d => append g (append d t2)
    end.

Compute append (N L (N L L))  L.

Theorem size_append : forall (t1 t2:bin), size (append t1 t2) = size t1 + size t2 + 1.
induction t2.
simpl.
intro t2.
ring.
intro t2.
simpl.
rewrite IHt2_1.
rewrite IHt2_2.
ring.
Qed.


Require Import List.
Check 1::2::nil.

Fixpoint build_list (n:nat) : list nat :=
match n with
| O => nil
| S x => x :: build_list x
end.

Compute build_list 5.

Lemma popo : forall A B : Prop, (A\/B) /\ ~A -> B.
intros A B H.
destruct H as [[ha | hb] na].
destruct na.
exact ha. exact hb.
Qed.

Lemma pupu : forall A :Prop, ~ (A/\~A).
intros.
unfold not.
intros. destruct H as [h1 h2]. 
apply h2 in h1. exact h1.
Qed.

Inductive bin_nat : Type :=
| Ln (n:nat): bin_nat
| Nn (n:nat): bin_nat -> bin_nat -> bin_nat .

Definition foo : bin_nat := Nn 3 (Ln 2) (Ln 5).

Print foo.

Fixpoint sum (t:bin_nat):nat :=
    match t with
    | Ln n => n
    | Nn n1 t1 t2 => n1 + sum t1 + sum t2
    end.

Fixpoint fibo (n:nat) {struct n}: nat :=
    match n with 
    | 0 =>  0
    | 1 =>  1
    | S n' =>  (fibo n') + (fibo (n'-1))
    end.

Fixpoint fib (n:nat) {struct n}: bin_nat :=
    match n with 
    | 0 => Ln 0
    | 1 => Ln 1
    | S n' => Nn (fibo n) (fib n') (fib (n'-1))
    end.

Definition flat_fib (t1 t2:bin_nat) : bin_nat :=
    match t1,t2 with
    | Ln s1, Ln s2 => Nn (s1 + s2) (Ln s1) (Ln s2)
    | Ln s1, Nn s2 g d => Nn (s1 + s2) (Ln s1) (Nn s2 g d)
    | Nn s1 g1 d1, Ln s2  => Nn (s1 + s2) (Nn s1 g1 d1) (Ln s2)
    | Nn s1 g1 d1, Nn s2 g2 d2 => Nn (s1 + s2) (Nn s1 g1 d1) (Nn s2 g2 d2)
    end.


Fixpoint fib_super (n:nat):bin_nat :=
    match n with
    | 0 => Ln 0
    | 1 => Ln 1
    | S n' => flat_fib (fib_super n') (fib_super (n'-1))
    end.

(* Compute fib_super 8. *)

Lemma eq_fib12 : forall n:nat, fib n = fib_super n.
intros.
induction n.
simpl. reflexivity.
simpl. rewrite IHn. 
Admitted.

Check True.
Check False.
Check eq 2 2.

Theorem trois : @eq nat 3 3.
apply @eq_refl  .
Qed.

Print trois.

Theorem coco : False -> True.
intro.
induction H.
Qed.

Print coco.

Print False_ind.

CoInductive flux : Set := 
 Cons  : nat -> flux -> flux.
 
Inductive liste : Set :=
| Co : nat -> liste -> liste
| Ni : liste.

Definition hd (x:flux) := let (a,s) := x in a.

Definition tl (x:flux) := let (a,s) :=x in s.

CoFixpoint from (n:nat): flux := Cons n (from (S n)).

Definition entiers := from O.

Fixpoint approx (x:flux) (n:nat) {struct n} : liste :=
  match n with
  | O => Ni
  | S n' => match x with Cons a s  => Co a (approx s n') end
end.

Eval compute in (approx entiers 10 ).
