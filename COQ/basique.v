Require Import Bool Arith.
Require Import List String.
Import ListNotations.
Open Scope list_scope.
Open Scope string_scope.

Check [1;2;3;4].

Section Insertion_sort.

Fixpoint insert  (a:nat) (l : list nat) : list nat :=
  match l with
  | nil => a::nil
  | h::t => if (leb a h) then a::l
            else h::(insert a t)
  end.

Compute insert 6 [1;3;6;7;9].

Fixpoint tri (l: list nat) : list nat :=
  match l with
   | nil => nil
   | h::t => insert h (tri t)
  end.

Compute tri [5;2;9;2;8;7;5;6;2;9;4;1;0;55].

Inductive Triée : list nat -> Prop :=
 | l_0 : Triée []
 | l_1 : forall n, Triée [n]
 | l_2 : forall n1 n2 l, n1<=n2 -> Triée (n2::l) -> Triée (n1::n2::l).

Check Triée.

Definition Tri_spec (f : list nat -> list nat) :=
  forall l, let l' := f l in Triée l' .
  
 Check leb.
 