Require Import Nat.
Open Scope nat_scope.
Require Coq.extraction.Extraction.
Extraction Language OCaml.

Inductive int_list : Set :=
| nil : int_list
| cons : forall (x : nat), forall (xs : int_list), int_list
.

Notation " [ x ; .. ; y ] " := (cons x .. (cons y nil) ..).

Print int_list.
Print int_list_rec.

Fixpoint length (l: int_list) : nat :=
match l with
| nil => O
| cons _ xs => 1 + length xs
end.

Theorem length_nil : length nil = O.
Proof.
    simpl.
    reflexivity.
Qed.

Fixpoint map (f : nat -> nat) (l : int_list) : int_list :=
match l with
| nil => nil
| cons x xs => cons (f x) (map f xs)
end.

Compute map (fun (x : nat) =>  x + 1) (cons 1  (cons 2  (cons 3  nil))).

Theorem map_length : forall (f: nat -> nat) (l:int_list), length (map f l) = length l.
Proof.
    intros f l.
    induction l.
    simpl.
    reflexivity.
    simpl.
    rewrite -> IHl.
    reflexivity.
Qed.

Print map_length.

Extraction map.
Extraction int_list. 

Compute (fun x:bool =>  3 + 4).

CoInductive stream : Type :=
| Cons : forall (x : nat) (xs : stream), stream
.

CoFixpoint  ones : stream :=
Cons 1 (ones).

Definition hd (s :stream) : nat := 
    match s with
    | Cons x xs => x
    end.

Definition tl (s :stream) : stream := 
    match s with
    | Cons x xs => xs
    end. 

Fixpoint approx (n :nat) (s : stream) : int_list :=
    match n with
    | O => nil
    | S k => cons (hd s) (approx k (tl s))
    end.


Compute approx (S (S (S O))) (ones).

CoFixpoint entiers (n:nat): stream :=
Cons n (entiers (n + 1)).

Print ex.

Theorem  deux : ex (fun n => (n = 2)).
Proof.
    exists 2.
    reflexivity.
Qed.

Print deux.

Theorem  plus_grand : forall (n:nat), ex (fun m => (m = n + 1)).
Proof.
    intros n.
    exists (n + 1).
    reflexivity.
Qed.

Print plus_grand.
Section toto.

Variable A:Prop.

Check A.
Variable a:A.
Check a.

Compute 2+2.


    

