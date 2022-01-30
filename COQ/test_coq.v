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


Compute 2+2.

Check nat_ind.

Check (fun x:nat => x = x).

Check (O=O).
Check eq_refl O.

Theorem negaln: forall (n:nat), n=n.
Proof.
    intros.
    induction n.
    reflexivity.
    reflexivity.
Qed.

Print negaln.

Compute (nat_ind (fun n:nat => (n = n)) (eq_refl O) (fun (n:nat) (_ : n=n) => (eq_refl (S n)))).
  

Check nat_ind.

Theorem nplus0: forall n:nat, n+0 = n.
Proof.
    intros.
    induction n.
    reflexivity.
   simpl.
  rewrite -> IHn.
  reflexivity.
Qed.

Print nplus0.


Inductive ma_liste : Type :=
| vide : ma_liste
| const : forall (x : nat), forall (xs : ma_liste), ma_liste
.

Definition l1 := const 1 (const 2 (const 3 vide)).

Print l1.
Print ma_liste_ind.

Inductive bin:Type :=
| one : bin
| two : bin
.

Print bin_ind.

Print nat_ind.

Fixpoint F (P:nat-> Prop) (casdebase: P O) (casrecursif : forall n:nat, (P n -> P( S n))) (n:nat) :=
    match n with
    | O =>  casdebase
    | S k =>  casrecursif k (F P casdebase casrecursif k)
    end.
    
 
Check F.


Fixpoint sum_n (n:nat):= 
    match n with
    | O => 0
    | S k => n + sum_n k
    end.

Compute sum_n (S (S (S O))).

Theorem sum_n_first_terms: forall (n:nat), sum_n (S n) = (S n) + sum_n n.
Proof.
    intros.
    induction n.
    simpl.
    reflexivity.

    rewrite -> IHn.
    simpl.
    reflexivity.
Qed.




Print eq_ind_r.




