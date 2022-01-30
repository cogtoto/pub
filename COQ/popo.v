Require Import Utf8.
Require Import Arith.
Definition premier : nat -> Prop :=
  fun p:nat =>
    forall n:nat,
      n>1 -> n<p  ->  p mod n <> 0 .

Locate "mod". (* mod est une notation pour Nat.modulo*)

Print Nat.modulo. (* fait appel à "-" et "Nat.divmod" et "snd"... *)

(* Déplie trop de trucs:*)
Eval compute in (premier 3).

(* Mais tu peux utiliser des stratégies plus économes qui feront
(souvent) ce que tu veux: *)

(* call by value mais en dépliant seulement "premier": *)
Eval cbv [premier] in (premier 3).

(* ou encore: *)
Eval unfold premier in (premier 3).