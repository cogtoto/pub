Require Import List Bool .
Require Extraction.


Inductive even :nat -> Prop :=
| ev0 : even 0
| evs : forall n:nat,  even n -> even (S (S n)).

Theorem  even8 : even 8.
Proof.
	repeat constructor.
Qed.

Print even8.

Inductive poption (P:Prop): Set :=
| psome : P -> poption P
| pnone : poption P.

Definition pair : forall n:nat, poption (even n).
   refine (fix pair (n:nat): poption (even n):=
     match n return (poption (even n)) with
	| 0 => psome _ _	
	| S (S n') => 
	  match (pair n') with
	  | psome  _ p  => psome _ _
	  | pnone _ => pnone _
	  end
	| _ => pnone _
	end).	

apply ev0.	   
apply evs.
exact p.
Defined.

Print pair.
Extraction pair.

Print sig.

Theorem even2: even 2.
Proof.
constructor.
constructor.	
Qed.

Check (exist (fun (n:nat) => even n) 2 even2).

Inductive soption (S:Set): Set :=
| ssome : S -> soption S
| snone : soption S.

Check ( ssome {n:nat | even n} (exist (fun (n:nat) => even n) 2 even2)).
Check ( snone {n:nat | even n} ).

Require Import Ring ArithRing Lia.

Print Nat.add.

Theorem nplusO: forall n:nat, n+O=n.
Proof.
	intros n.
	induction n.
	simpl. reflexivity.
    simpl. rewrite -> IHn. reflexivity.
Qed.

Lemma nplussn : forall n:nat, n + S n = S (n + n).
Proof.
	intro n.
	induction n. reflexivity.
	ring.
Qed.

Theorem mult2: forall (n:nat), even (2*n).
Proof. 
	intros n.
	unfold mult .
    simpl.
		induction n.
	simpl.
	constructor.
	rewrite -> nplusO.
	rewrite  -> nplusO in IHn.
	apply evs in IHn.
	simpl. 
	rewrite -> nplussn.
	exact IHn.
Qed.

Definition fois2 (n:nat): {n:nat | even n} :=
	exist (fun (n:nat) => even n) (2*n) (mult2 n).

Compute fois2 3.

Print sumbool.
Check left I.
Theorem cinqdiffO : 5 <> O.
Proof.
auto.
Qed.

Definition pred_partial : forall n:nat, n<>O -> nat.
refine (fun n:nat => 
   match n with 
   | O => fun h:O<>O => _
   | S n' => fun h:S n' <> O => n'
   end).
   exact n.
   Defined.
 
Compute  (pred_partial 5) cinqdiffO.

Definition pred_partial2 : forall n:nat, n<>O -> nat.
intros n.
case n. intro h. elim h. reflexivity.
intros p h. exact p.
Defined.

Print pred_partial2.

Compute pred_partial2 5 cinqdiffO.


Theorem even4 : even 4.
Proof.
	repeat constructor.
Qed.

Print even4.

Section partial.
  Variable P : Prop.

Inductive partial : Set :=
  | Proved : P -> partial
  | Uncertain : partial.
End partial.

Notation "[ P ]" := (partial P) : type_scope.

Definition Check_even : forall n:nat, partial (even n) .
refine (
	fix F (n:nat): partial (even n) :=
	match n return (partial (even n)) with
	| O => Proved _ _
	| 1 => Uncertain _
	| S (S n') => if (F n') then Proved _ _ else Uncertain _
	end
).
constructor.
constructor. exact e.
Defined.

Definition PartialOut (P:Prop) (x : partial P) :=
	match x return (match x with | Proved _ _ => P | Uncertain _  => True end) with
	| Proved _ pf => pf
	| Uncertain  _=> I
	end.


Theorem even'8 : even 8.
Proof.
	exact (PartialOut (even 8) (Check_even 8)).
Qed.

Print even'8.

Lemma nplusO' : forall n:nat, n+O=n.
Proof.
	intros n.
	induction n.
	simpl. reflexivity.
	simpl. rewrite -> IHn. reflexivity.
Qed.

Search  ( _<=_+_).

Print PeanoNat.Nat.add_nonneg_nonneg.

Theorem toto: forall (n:nat) (m:nat), n>0 -> m>0 -> n+m>0.
Proof.
	intros.
	unfold gt.
    unfold lt.

	unfold gt in H. unfold lt in H.
	unfold gt in H0. unfold lt in H0.

	lia.
Qed.

Print toto.
		
