Definition id (A:Set) := fun (x:A) => x.

Inductive nat : Set :=
| O : nat
| S (_:nat) : nat.

Check S O.


Check 3.

Compute (id nat (S (S O))).

Check id.

Theorem ident: forall A:Set, A->A .
Proof.
  intros. exact H.
Qed.
Print ident.

Inductive liste (A:Set) : Set :=
 | nil :  liste A
 | cons : A -> liste A ->  liste A.

Fixpoint longueur (A:Set) (l:liste A) : nat := 
  match l with
  | nil _ => O
  | cons _ a b  => S (longueur A b)
  end.

Compute longueur nat (cons nat 3 (cons nat  2 (nil nat))).

Theorem l_th: forall (A:Set) (l:liste A) (a:A), longueur A (cons A a l) = S (longueur A l) .
Proof.
intros. simpl. reflexivity.
Qed.

Definition  tete (A:Set) (l: liste A) : (option A) :=
  match l with
  | nil _ => None
  | cons _ a _ => Some a
  end.
  
Compute tete nat (cons nat 3 (cons nat 2 (nil nat))).

Inductive tete_correct (A:Set) : (forall A:Set, liste A -> option A) -> Prop :=
   | tete_correct0 : forall (f:forall A:Set,  liste A -> option A), (f A (nil  A ) = None ) -> tete_correct A f
   | tete_correct1 : forall (f:forall A:Set, liste A -> option A) (a:A) (l:liste A), (f A (cons A a l) = Some a) -> tete_correct A f.
   

(** Theorem hd_correct : forall A:Set, tete_correct A tete . *)

Require Import List.
Section les_streams.

CoInductive stream (A:Set): Set :=
 | Cons : A -> stream A-> stream A.

 CoFixpoint zeroes : stream nat := Cons nat O zeroes.

Fixpoint approx (A:Set) (s: stream A) (n : nat) : list A :=
  match n with
  | O => nil 
  | S n' => 
      match s with 
        | Cons _ h t =>  h :: approx A t n'
      end
  end.

 Check approx.

Compute approx nat zeroes 10.