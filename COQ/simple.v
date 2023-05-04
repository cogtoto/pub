(* tests COQ *)

Check (fun (x:nat) => I). 

Inductive vide : Set := .

Inductive mon_faux : Prop := .
Inductive m1 : Prop :=
| False
| True.
 
Inductive coco : Set :=
| C1
| C2.
Print coco_ind.
Theorem coco_trivial : forall (c:coco), c=C1 \/ c=C2.
intro.
induction c.
Show Proof.
left. reflexivity.
right. reflexivity.
Qed.

Definition mapreuve (c:coco): c=C1 \/ c=C2 :=
match c with
 | C1 => or_introl (eq_refl C1)
 | C2 => or_intror (eq_refl C2) 
 end.

Print coco_ind.

Definition mapreuve2 :forall c:coco, c=C1 \/ c=C2 :=
	coco_ind (fun x:coco => x = C1 \/ x= C2) (or_introl (eq_refl C1)) (or_intror (eq_refl C2))
	.




Print mapreuve.
Theorem coco_trivial2 : forall (c:coco), c=C1 \/ c=C2.
intro.
apply (mapreuve c).
Qed.

Theorem tout : 42=42.
exact (eq_refl 42).
Qed.

Print tout.
Print eq_refl.
Print coco_trivial.
Print or_introl.
Check eq_refl.
Print "=".
Print or_introl.

Print coco_rect.

Print mon_faux_ind.
Print False_ind.
Print False.


Theorem abssurde : forall (x:vide), 2=3.
destruct 1.
Qed.

Theorem absurde :  vide -> 2=3.
intro.
induction H.
Qed.


Print absurde.


Theorem thm_eq_sym : (forall x y : Set, x = y -> y = x).
intros.

destruct H as [].
exact (eq_refl x).
Qed.

Print thm_eq_sym.
Print eq.

Compute  ((fun x=>2) 3). 

Theorem popo: ((fun x => 3) 2) = 3 .
Proof.
	simpl.
	exact (eq_refl :3=3 ).
Qed.

Print popo.

Require Import Nat.
Print Nat.

Check 3.

Inductive ma_liste  :=
| Nil : ma_liste
| Cons (n:nat) (l:ma_liste) : ma_liste.

Definition cucu : ma_liste := Cons 3 (Cons 4 Nil).

Print cucu.

Definition tete (l:ma_liste) : nat :=
	match l with
	| Nil => 0
	| Cons n _ => n
	end.

Infix "::" := Cons (right associativity, at level 60).

Definition coco := 42 :: 42 :: 44 :: 21 :: Nil.
Print coco.
Definition tete2 (l:ma_liste) (p:l<>Nil) :nat :=
       match l as b return (l=b ->nat ) with
       | Nil =>  (fun foo : l = Nil => match (p foo) return nat with end )
       | Cons n _ => ( fun  foo :l = Cons n _ => n)
       end eq_refl.

Locate "<>".

Theorem yop : Cons 2 Nil <> Nil.
unfold "<>".
intros.
discriminate.
Qed.

Print yop.
Print False_ind.
Print eq_ind.

Compute tete2 (Cons 2 Nil) yop.