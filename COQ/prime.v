(* prime numbers *)

Require Import Arith Lia.

Fixpoint check_range (n:nat) (r:nat) : bool :=
match r with 
| 0 => true | 1 => true 
| S r' => if (n mod r)=?0 then false else check_range n r'
end.

Definition check_premier (n:nat) : bool :=
    check_range n (n-1).

Definition is_prime : nat -> Prop :=
    fun p:nat => forall n:nat, n<>1 -> n < p -> p mod n <> 0.

Theorem check_is_true : forall p:nat, check_premier p = true ->  is_prime p.
intros.
unfold check_premier in H. unfold check_range in H. simpl in H.
unfold is_prime. intros.

Admitted.


Theorem premier2333: is_prime 2333 .
exact (check_is_true 2333 eq_refl).
Qed.

Print premier2333.

Print refl_equal.


