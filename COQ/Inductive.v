Inductive bin:Set :=
| un:bin
| deux:bin
.

Print bin_ind.

Inductive entiers:Set :=
| zero:entiers
| succ:entiers -> entiers .

Print entiers_ind.

Print entiers_rec.

Print sig.
Print proj1_sig.

Print proj2_sig.


Definition  zzero (n:nat) := {n:nat | n = 0}.

Check (exist (fun (n:nat) => n=O) O eq_refl).
Compute fst (1,3).



Definition div_eucl (a:nat) (b:nat) := {p:nat*nat   | a = b*(fst p)+(snd p) /\  (snd p) < b}.

Theorem  septdeux : 7 = 2*(fst (3,1)) + 1 /\ (snd (3,1)) < 2.
Proof.
	auto.
Qed.

Check (exist (fun (p:nat*nat) => 7=2*(fst p)+(snd p) /\ (snd p) < 2) (3,1) septdeux).

Compute proj1_sig (exist (fun (p:nat*nat) => 7=2*(fst p)+(snd p) /\ (snd p) < 2) (3,1) septdeux).

Print Nat.pred.
Require Import Extraction.

Extraction Nat.pred.

Lemma zerosupzero : 0 > 0 -> False.
Proof.
unfold gt.
unfold lt.
intros.
inversion H.
Qed.

Definition pred2 (n:nat) : (n>0) -> nat :=
	match n with
	| O => fun pf : O > O => match zerosupzero pf with end
	| S n' => fun _ => n'
	end.

Theorem troissup0 : 3 > O.
Proof.
	unfold gt.
	unfold lt.
	auto.
Qed.




Print or_introl.


Definition  pred (n:nat): {m:nat | S m = n \/ m=O} :=
	match n  as n0 return ({m : nat | S m = n0  \/ m=O}) with
	| O => exist  (fun m:nat => S m= O \/ m=0 ) O  (or_intror (eq_refl O))
	| S n' =>  exist  (fun m:nat => S m = S n' \/ m=0)  n'  (or_introl (eq_refl (S n')))
	end.

Compute pred O.

Definition  pred3 (n:nat): {m:nat | S m = n \/ m=O} :=
	match n  with
	| O => exist  (fun m:nat => S m= O \/ m=0 ) O  (or_intror (eq_refl O))
	| S n' =>  exist  (fun m:nat => S m = S n' \/ m=0)  n'  (or_introl (eq_refl (S n')))
	end.

Extraction pred.

Check @eq_refl.

Compute (@eq_refl nat 4).

Compute eq_refl.

Definition two :nat.
refine (_:nat).
exact 2.
Defined.

Print two.

Definition  pred5 (n:nat): {m:nat | S m = n \/ m=O}. 
	refine 
	match n  as n0 return ({m : nat | S m = n0  \/ m=O}) with
	| O => exist  (fun m:nat => S m= O \/ m=0 ) O  _
	| S n' =>  exist  _  n'  (or_introl (eq_refl (S n')))
	end.

auto.
Qed.

Print pred5.

