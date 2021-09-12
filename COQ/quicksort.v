Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.
Require Import Coq.Bool.Bool.
Require Import Coq.Structures.OrdersFacts.
Require Import Omega.
Require Import Coq.Logic.ClassicalFacts.


(*The recursion tree for quickSort*)
Inductive qs_tree : list nat -> Type :=
| qs_tree_base : qs_tree nil
| qs_tree_step : forall ( x: nat) (xs: list nat), 
  qs_tree (filter (fun y => leb y  x) (xs)) -> 
  qs_tree (filter (fun y => negb (leb y x)) (xs)) -> 
    qs_tree (cons x xs).

(*The fixpoint which evaluates a recursion tree*)
Fixpoint qs_helper (l : list nat) (q : qs_tree l) {struct q} : list nat :=
  match q with
  | qs_tree_base => nil
  | qs_tree_step x xs q0 q1 =>
      (qs_helper (filter (fun y : nat => leb y x) xs) q0) ++ (x ::
        (qs_helper (filter (fun y : nat => negb (leb y x)) xs) q1))
  end.

Require Import Coq.Sorting.Sorted.

(*Create the recursion tree for a given list*)
  (*Redefine lemmas from libraries to be opaque*)
Lemma leb_correct : forall m n:nat, m <= n -> leb m n = true.
Proof.
  induction m as [| m IHm]. trivial.
  destruct n. intro H. elim (le_Sn_O _ H).
  intros. simpl in |- *. apply IHm. apply le_S_n. assumption.
Defined.

Lemma leb_correct_conv : forall m n:nat, m < n -> leb n m = false.
Proof.
  intros.
  generalize (leb_complete n m).
  destruct (leb n m); auto.
  intros.
  elim (lt_irrefl _ (lt_le_trans _ _ _ H (H0 (refl_equal true)))).
Defined.

Lemma not_true_is_false : forall b:bool, b <> true -> b = false.
destruct b.
intros.
red in H; elim H.
reflexivity.
intros abs.
reflexivity.
Defined.

Lemma gt_leb_conv: forall (a: nat) (a0 : nat),(a > a0) -> (a0 <= a).
Proof.
intros.
intuition.
Defined.

  (*New facts about filters*)
Lemma filter_commutative: forall (xs : list nat) (f1 : nat -> bool) (f2 : nat -> bool),
   (filter f2 (filter f1 xs))
= (filter f1 (filter f2 xs)).
Proof.
intros.
induction xs.
simpl. (*base case*)
intuition.
simpl. (*step*)
destruct (bool_dec (f1 a) true).
destruct (bool_dec (f2 a) true).
rewrite e.
rewrite e0.
simpl.
rewrite e.
rewrite e0.
rewrite IHxs.
intuition. (*case f1,!f2*)
apply not_true_is_false in n.
rewrite n; rewrite e.
simpl.
rewrite n.
intuition. (*case !f1,f2*)
apply not_true_is_false in n.
destruct (bool_dec (f2 a) true).
rewrite n;rewrite e.
simpl.
rewrite n.
intuition.
apply not_true_is_false in n0.
rewrite n;rewrite n0.
intuition.
Defined.

Lemma filter_combine: forall (xs : list nat) (f1 :nat -> bool) (f2 : nat -> bool),
filter f1 (filter f2 xs) = filter ( fun x: nat => (f1 x) && f2 x) xs.
Proof.
intros.
induction xs.
simpl.
reflexivity.
simpl.
destruct (bool_dec (f1 a) true); intros. (*case f1,f2*)
destruct (bool_dec (f2 a) true); intros.
rewrite e; rewrite e0.
simpl.
rewrite e.
rewrite IHxs.
intuition. (*case f1,!f2*)
apply not_true_is_false in n as e0.
rewrite e; rewrite e0.
simpl.
intuition. (*case !f1,f2*)
apply not_true_is_false in n as e.
destruct (bool_dec (f2 a) true); intros.
rewrite e; rewrite e0.
simpl.
rewrite e.
intuition. (*case !f1,!f2*)
apply not_true_is_false in n0 as e0.
rewrite e; rewrite e0.
intuition.
Defined.

  (*lemma f1 -> f2 then (filter f2 (filter f1 xs)) = filter f2 xs*)
  (*made specific for convienience*)
Lemma filter_nat_helper_right: forall (a : nat) (b :nat) (xs : list nat), (a>=b)-> 
  (filter (fun x0 : nat => negb(x0 <=? a))) (filter (fun x0 : nat => negb(x0 <=? b)) xs)=
(filter (fun y : nat => negb(y <=? a)) xs).
Proof.
intros.
induction xs. (*induction xs*)
simpl. (*base case*)
intuition.
simpl. (*step*)
destruct (le_gt_dec a0 b). (*case a0 <= b*)
apply leb_correct in l as l1.
rewrite l1; simpl.
apply (le_trans a0 b a) in l.
apply leb_correct in l.
rewrite l; simpl.
intuition.
intuition. 
apply leb_correct_conv in g. (*case a0 > b*)
rewrite g; simpl.
destruct (le_gt_dec a0 a).
apply leb_correct in l.
rewrite l; simpl.
intuition.
apply leb_correct_conv in g0.
rewrite g0; simpl.
rewrite IHxs.
intuition.
Defined.

Lemma filter_nat_helper_left: forall (xs : list nat) (a:nat) (b:nat),
  (b > a) -> (filter (fun y : nat => (y <=? a) && (y <=? b)) xs) = filter (fun y : nat => y <=? a) xs.
Proof.
intros.
induction xs.
simpl. (*base case*)
intuition.
simpl. (*step*)
destruct (le_lt_dec a0 a). (*case a0 <= a*)
apply (gt_le_trans b a a0) in H.
apply leb_correct in l.
rewrite l.
apply gt_leb_conv in H.
apply leb_correct in H.
rewrite H.
rewrite andb_true_l.
rewrite IHxs.
intuition.
intuition. (*case a < a0*)
apply leb_correct_conv in l.
rewrite l.
rewrite andb_false_l.
intuition.
Defined.

(*Vital lemma: convert list to recursion tree for quickSort*)
Lemma create_qs_tree: forall (l : list nat), (qs_tree l).
Proof.
intros.
induction l. (*induction l*)
constructor. (*base*)
constructor. (*step*)
induction IHl. (*induction on IH : qs_tree*)
constructor. (*base*)
simpl. (*step left side*)
destruct (le_lt_dec x a). (*case x in left branch*)
apply leb_correct in l.
rewrite l.
constructor. (*recurse*)
rewrite filter_commutative. (*left recursion branch*)
intuition.
rewrite filter_commutative. (*right recursion branch*)
intuition. 
apply leb_correct_conv in l as l2. (*if head xs not left*)
rewrite l2.
apply (filter_nat_helper_left xs a x) in l.
rewrite filter_combine in IHIHl1.
rewrite <- l.
intuition.
induction IHl. (*recurse right branch*)
simpl.
constructor.
simpl.
destruct (le_gt_dec x a). (*if head xs not in right branch*)
apply leb_correct in l as l0;
rewrite l0.
simpl.
intuition.
apply (filter_nat_helper_right a x xs) in l.
rewrite <- l.
intuition. (*if head xs in right branch*)
apply leb_correct_conv in g.
rewrite g.
simpl.
constructor. (*recurse*)
rewrite filter_commutative.
intuition.
rewrite filter_commutative.
intuition.
Defined.

Print create_qs_tree.

Compute create_qs_tree (1::(2::nil)).

