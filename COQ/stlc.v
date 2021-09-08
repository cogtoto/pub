(***************************************************************************
* Simply Type Lambda Calculus in COQ			                   *
***************************************************************************)

Set Implicit Arguments.
Require Import String Nat.
Open Scope string_scope.

(* ********************************************************************** *)
(** * Definitions *)
(** ** Grammars *)

(** Grammaire des types. Nous avons deux constructeurs, un pour les types de variable and un
 pour les types des abstractions: le type flèche de la forme  [T1 -> T2].
*)

Inductive typ : Set :=
  | typ_var   : string -> typ
  | typ_arrow : typ -> typ -> typ.

(** Grammar of pre-terms. We use a locally nameless representation for the
    simply-typed lambda calculus, where bound variables are represented as 
    natural numbers (de Bruijn indices) and free variables are represented as
    atoms. 
*)

Inductive terme : Set :=
  | bvar : nat -> terme
  | fvar : string -> terme
  | abs  : terme -> terme
  | app  : terme -> terme -> terme.

Coercion bvar : nat >-> terme.
Coercion fvar : string >-> terme.

Definition t1 := abs (app "Y" 0).
Compute t1.

(** Un autre exemple avec le terme $t_2 = \lambda x.\lambda y. (y x)) *)
Definition t2 := abs (abs (app 0 1)).

(* ********************************************************************** *)
(** ** Opening *)

(** Opening replaces an index with a term. It corresponds to informal
    substitution for a bound variable, such as in the rule for beta
    reduction.
*)
Open Scope nat.

Fixpoint open_rec (k : nat) (u : terme) (t : terme) {struct t} : terme :=
  match t with
  | bvar i    => if k =? i then u else (bvar i)
  | fvar x    => fvar x
  | abs t1    => abs (open_rec (S k) u t1)
  | app t1 t2 => app (open_rec k u t1) (open_rec k u t2)
  end.

Definition open t u := open_rec 0 u t.

Notation "{ k ~> u } t" := (open_rec k u t) (at level 67).
Notation "t ^^ u" := (open t u) (at level 67).
Notation "t ^ x" := (open t (fvar x)).

Lemma demo_open :
  open (app (abs (app 1 0)) 0) "Y" =
       (app (abs (app "Y" 0)) "Y").
Proof.
  unfold open. unfold open_rec. auto.
Qed.


(* ********************************************************************** *)
(** ** Semantics *)

(** We now define the semantics of our call-by-value lambda calculus. 
    We define values and small-step reduction. 
*)

Inductive valeur : terme -> Prop :=
  | valeur_abs : forall (t1: terme), valeur (abs t1).

Inductive red : terme -> terme -> Prop :=
  | red_beta : forall (t1 t2:terme),
      valeur t2 ->
      red (app (abs t1) t2) (t1 ^^ t2)
  | red_app_1 : forall t1 t1' t2 :terme,
      red t1 t1' ->
      red (app t1 t2) (app t1' t2)
  | red_app_2 : forall t1 t2 t2' :terme,
      valeur t1 ->
      red t2 t2' ->
      red (app t1 t2) (app t1 t2').

(** We use the notation [t --> t'] to denote small step reduction. *)

Notation "t --> t'" := (red t t') (at level 68).

(* ********************************************************************** *)
(** ** Environments et contexte*)
    
Definition ctx := list (string * typ).
Open Scope list_scope.
Module ListNotations.
Notation " [ ] " := nil : list_scope.
Notation " [ x ] " := (cons x nil) : list_scope.
Notation " [ x ; .. ; y ] " := (cons x .. (cons y nil) ..) : list_scope.
Notation " s1 & s2 " := (Datatypes.app s1 s2) (at level 67)  : list_scope.
End ListNotations.

Import ListNotations.
Definition e1 :ctx := [ ("v1", typ_var "entier") ].
Definition e2 :ctx := [ ("v2", typ_var "entier") ].

Compute e1 & e2 .


(* ********************************************************************** *)
(** ** Typage *)

(** If [E] and [F] are two contexts, then [E & F] denotes their 
    concatenation. If [x] is a variable and [T] is a type, then 
    [(x ~ T] denotes a singleton environment where [x] is bound to [T].
    In particular, [E & x ~ T] denotes a context [E] extended 
    with a binding from [x] to [T]. The empty environment is 
    called [empty].
*)


(** The ternary predicate [binds] holds when a given binding is
    present in an environment.  More specifically, [binds x T E] holds
    when the last binding of [x] binds [x] to the type [T]. *)

Fixpoint binds (x:string) (T:typ) (E:ctx) {struct E} : Prop :=
  match E with
  | [] => False
  | (v,t) :: r => (x=v /\ T=t) \/ binds x T r
end.

Compute binds "v1" (typ_var "entier") e1.
Compute e1.

Theorem b1 : binds "v1" (typ_var "entier") e1.
Proof.
	simpl.
       left.
       auto.
Qed.

Reserved Notation "E |= t ~: T" (at level 69).

Inductive typing : ctx -> terme -> typ -> Prop :=
  | typing_var : forall E x T,
      binds x T E ->
      E |= (fvar x) ~: T
  | typing_abs : forall  E U T t1,
      forall x,  
        (E & [(x , U)] |= t1 ^ x ~: T) ->
      E |= (abs t1) ~: (typ_arrow U T)
  | typing_app : forall S T E t1 t2,
      E |= t1 ~: (typ_arrow S T) -> 
      E |= t2 ~: S ->
      E |= (app t1 t2) ~: T

where "E |= t ~: T" := (typing E t T).

(* ********************************************************************** *)
(** ** Statement of theorems *)

Definition preservation_statement := forall E t t' T,
  E |= t ~: T ->
  t --> t' ->
  E |= t' ~: T.

Definition progress_statement := forall t T, 
  nil |= t ~: T ->
     valeur t 
  \/ exists t', t --> t'.


(** * Infrastructure *)

(** Before we start getting into the proofs, we need to set up a few
    more things.
     - functions definining free variables and substitution,
     - tactics to pick fresh names and to handle freshness-related goals,
     - a few axioms about the behavior of operations on terms.

    We will purposely introduce some axioms, so that we can go straight
    to the proofs that we are interested in. Once we are finished with
    the main proofs, we will do a second pass in order to turn the 
    axioms into proper lemmas.
*)

(* ********************************************************************** *)
(** ** Free variables *)

(** The function [fv], defined below, calculates the set of free
    variables in an expression.  Because we are using locally nameless
    representation, where bound variables are represented as indices,
    any name we see is a free variable of a term.  In particular, this
    makes the [trm_abs] case simple.
*)
Open Scope string.

Fixpoint mem  (x:string) (l:list string) : bool :=
 match l with
 | nil => false
 | h::t => if h=?x then true else mem x t 
 end.


Fixpoint union (l1 l2: list string) : list string :=
  match l1 with
    | a1::r1 => if mem a1 l2 then union r1 l2
                else  a1 :: (union r1 l2)
    | nil => l2
    end.


Fixpoint fv (t : terme) {struct t} : list string :=
  match t with
  | bvar i    => nil
  | fvar x    => [x]
  | abs t1    => (fv t1)
  | app t1 t2 => (union (fv t1) (fv t2))
  end.

(* ********************************************************************** *)
(** ** Substitution *)

(** Substitution replaces a free variable with a term.  The definition
    below is simple for two reasons:
      - Because bound variables are represented using indices, there
        is no need to worry about variable capture.
      - We assume that the term being substituted in is locally
        closed.  Thus, there is no need to shift indices when
        passing under a binder.
*)

Fixpoint subst (z : string) (u : terme) (t : terme) {struct t} : terme :=
  match t with
  | bvar i    => bvar i
  | fvar x    => if x =? z then u else (fvar x)
  | abs t1    => abs (subst z u t1)
  | app t1 t2 => app (subst z u t1) (subst z u t2)
  end.

(** We define a notation for free variable substitution that mimics
    standard mathematical notation. *)

Notation "[ z ~> u ] t" := (subst z u t) (at level 68).

(** Below is a demo. *)


Lemma demo_subst1:  ["Y" ~> "Z"] (abs (app 0 "Y")) = (abs (app 0 "Z")).
Proof.
	simpl.
	auto.
Qed.



(* ********************************************************************** *)
(** ** Tactics *)

(** When picking a fresh atom or applying a rule that uses cofinite
    quantification, choosing a set of atoms to be fresh for can be
    tedious.  In practice, it is simpler to use a tactic to choose the
    set to be as large as possible.

    The first tactic we define, [gather_vars], is used to collect
    together all the atoms in the context.  It relies on an auxiliary
    tactic from [LibLN_Tactics], [gather_vars_with], which collects
    together the atoms appearing in objects of a certain type.  The argument 
    to [gather_vars_with] is a function that should return the set of
    vars appearing in its argument. 
*)

Ltac gather_vars :=
  let A := gather_vars_with (fun x : vars => x) in
  let B := gather_vars_with (fun x : var => \{x}) in
  let C := gather_vars_with (fun x : ctx => dom x) in
  let D := gather_vars_with (fun x : trm => fv x) in
  constr:(A \u B \u C \u D).

(** The tactic [pick_fresh_gen L x] creates a new atom fresh 
    from [L] and called [x]. Using the tactic [gather_vars],
    we can automate the construction of [L]. The tactic
    [pick_fresh x] creates a new atom called [x] that is fresh
    for "everything" in the context.
*)

Ltac pick_fresh x :=
  let L := gather_vars in (pick_fresh_gen L x).

(** The tactic [apply_fresh T as y] takes a lemma T of the form 
    [forall L ..., (forall x, x \notin L, P x) -> ... -> Q.]
    and applies it by instantiating L as the set of variables 
    occuring in the context (L is computed using [gather_vars]).
    Moreover, for each subgoal of the form [forall x, x \notin L, P x]
    being generated, the tactic automatically introduces the name [x] 
    as well as the hypothesis [x \notin L].
*)

Tactic Notation "apply_fresh" constr(T) "as" ident(x) :=
  apply_fresh_base T gather_vars x.

(** The tactic [apply_fresh* T as y] is the same as 
    [apply_fresh T as y] except that it calls [intuition eauto]
    subsequently. It is also possible to call [apply_fresh]
    without specifying the name that should be used.
*)

Tactic Notation "apply_fresh" "*" constr(T) "as" ident(x) :=
  apply_fresh T as x; auto*.
Tactic Notation "apply_fresh" constr(T) :=
  apply_fresh_base T gather_vars ltac_no_arg.
Tactic Notation "apply_fresh" "*" constr(T) :=
  apply_fresh T; auto_star.


(* ********************************************************************** *)
(** ** Automation *)

(** Automation is crucial for avoiding to have hundreds of subgoals to
    handle by hand. For the tactics [auto] and [eauto] to be able to
    derive proof automatically, we need to give explicitely the list of
    lemmas that the proof search algorithm should try to exploit.
    The command [Hint Resolve lemma] adds a given lemma to the database
    of proof search. The command [Hint Constructors ind] is equivalent
    to invoking [Hint Resolve] on all of the constructors of the inductive
    type [ind]. We use [Hint Constructors] for all our inductively-defined
    predicates.
*)

Hint Constructors term value red.


(* ********************************************************************** *)
(** ** Axiomatization of the infrastructure *)

Module AxiomatizedVersion.

(** At the point, we introduce two simple axioms and skip the many
    uninteresting auxiliary lemmas that would be required to prove them.

    The first axiom states that substitution for a variable [x] 
    commutes with the operation of opening with another variable [y].

    The second axiom states that the opening of a term [t] with a 
    term [u] can be decomposed in two steps: first opening [t] with
    a variable [x], and second substituting [u] for [x].
*)

Axiom subst_open_var : forall x y u t,
  y <> x -> term u ->
  ([x ~> u]t) ^ y = [x ~> u] (t ^ y).

Axiom subst_intro : forall x t u, 
  x \notin (fv t) -> term u ->
  t ^^ u = [x ~> u](t ^ x).

(** In order to focus our complete attention on the interesting proofs
    first, we add a meta-axiom to tell Coq that it should admit any
    subgoal related to well-formedness, i.e., any goal of the form
    [term t] or [ok E]. We will remove these axioms later on.
    This meta-axiom takes the form of a hint whose action is [skip].
    This hint will be triggered whenever we call [auto]. The [skip]
    tactics simply admits the current goal. *)

Local Hint Extern 1 (term _) => skip.
Local Hint Extern 1 (ok _) => skip.

(** It might also be useful to add an extra meta-axiom, to get rid of
    all the freshness-related subgoals. We do not need here, though. 

    [Hint Extern 1 (_ \notin _) => skip]
*)

(* ********************************************************************** *)
(* ********************************************************************** *)
(* ********************************************************************** *)
(** * Proofs *)

(** Weakening states that if an expression is typeable in some
    environment, then it is typeable in any well-formed extension of
    that environment.  This property is needed to prove the
    substitution lemma.

    As stated below, this lemma is not directly proveable.  The natural
    way to try proving this lemma proceeds by induction on the typing
    derivation for [t].
*)

Lemma typing_weaken_0 : forall E F t T,
   E |= t ~: T -> 
   ok (E & F) ->
   (E & F) |= t ~: T.
Proof.
  introv Typ. induction Typ; intros Ok; subst.
  apply* typing_var.
  apply_fresh* typing_abs as y. (* stuck here *)
Admitted.

(** We are stuck in the [typing_abs] case because the induction
    hypothesis [H0] applies only when we weaken the environment at its
    head.  In this case, however, we need to weaken the environment in
    the middle; compare the conclusion at the point where we are stuck
    to the hypothesis [H], which comes from the given typing derivation.

    We can obtain a more useful induction hypothesis by changing the
    statement to insert new bindings into the middle of the
    environment, instead of at the head.  However, the proof still
    gets stuck, as can be seen by examining each of the cases in
    the proof below. 
*)

Lemma typing_weaken_2 : forall G E F t T,
   (E & G) |= t ~: T -> 
   ok (E & F & G) ->
   (E & F & G) |= t ~: T.
Proof.
  introv Typ.
  (* because of limitations to the [induction] tactic,
     (limitations not entirely solved by [dependent induction]), 
     we need to manually generalize the parameters of the
     judgment that we perform the induction on. *)
  gen_eq H: (E & G). gen G.
  induction Typ; intros G EQ Ok; subst.
  apply* typing_var. apply* binds_weaken.
  (* --begin case abs-- *)
  (* first we compute [L'], the set of used variables *)
  let L := gather_vars in sets L': L.
  (* now we apply [typing_abs] using [L'] *)
  apply (@typing_abs L').
  (* we can introduce a name [y] such that [y \notin L'] *)
  intros y Fry. subst L'.
  (* to apply the induction hypothesis, we need to rewrite
     the context for associativity *)
  rewrite <- concat_assoc.
  (* now we can apply the induction hypothesis *)
  apply H0.
    auto. (* we can prove [y \notin L] *)
    rewrite concat_assoc. auto.
    rewrite concat_assoc. auto.
  (* --end case abs-- *)
  apply* typing_app.
Qed.

(** Using the tactic [apply_fresh] introduced earlier, as well as
    the tactic [apply_ih_bind] which is specialized for applying an
    induction hypothesis up to rewriting of associativity in contexts,
    we obtain a nice and short proof. *)

Lemma typing_weaken : forall G E F t T,
   (E & G) |= t ~: T -> 
   ok (E & F & G) ->
   (E & F & G) |= t ~: T.
Proof.
  introv Typ. gen_eq H: (E & G). gen G.
  induction Typ; intros G EQ Ok; subst.
  apply* typing_var. apply* binds_weaken.
  apply_fresh* typing_abs as y. apply_ih_bind* H0.
  apply* typing_app.
Qed.

(** Proving that typing is preserved by substitution involves very
    similar techniques. The only non trivial part concerns the case
    analysis in the variable case. For that, we use the tactics
    [binds_get] and [binds_cases] which extract information from
    [binds] hypotheses. *)

Lemma typing_subst_1 : forall F E t T z u U,
  (E & z ~ U & F) |= t ~: T ->
  E |= u ~: U ->
  (E & F) |= [z ~> u]t ~: T.
Proof.
  introv Typt Typu. gen_eq G: (E & z ~ U & F). gen F.
  induction Typt; intros G Equ; subst; simpl subst.
  case_var. (* test whether [x] is [z] or not *)
    binds_get H0. (* [T] must be [U] *)
      (* here we want to exploit [typing_weaken] with [G]
         instantiated as [empty], but this is not easy
         because we need to argue that [E & empty = E] *)
      lets M: (@typing_weaken empty E G u U).
      do 2 rewrite concat_empty_r in M.
      apply* M.
    binds_cases H0. 
      apply* typing_var. (* if [x] in [G] *)
      apply* typing_var. (* if [x] in [E] *)
  apply_fresh typing_abs as y.
   rewrite* subst_open_var. apply_ih_bind* H0.
  apply* typing_app.
Qed.

(** As we have seen in the proof above, specializing lemmas 
    on empty environments can be quite tedious. Fortunately,
    the metatheory library includes tactic that greatly helps.
    Calling [apply_empty lemma] is almost equivalent to calling
    [apply (@lemma empty)] except that it rewrites away the
    empty environments on the fly. The proof becomes as follows.
*)

Lemma typing_subst : forall F E t T z u U,
  (E & z ~ U & F) |= t ~: T ->
  E |= u ~: U ->
  (E & F) |= [z ~> u]t ~: T.
Proof.
  introv Typt Typu. gen_eq G: (E & z ~ U & F). gen F.
  induction Typt; intros G Equ; subst; simpl subst.
  case_var.
    binds_get H0. apply_empty* typing_weaken.
    binds_cases H0; apply* typing_var.
  apply_fresh typing_abs as y.
   rewrite* subst_open_var. apply_ih_bind* H0.
  apply* typing_app.
Qed.

(** The proof of preservation appears below.
    Proof sketch: By induction on the typing derivation for [t].

      - [typing_var] case: Variables don't step.

      - [typing_abs] case: Abstractions don't step.

      - [typing_app] case: By case analysis on how [t] steps. The
        [eval_beta] case is interesting, since it follows by the
        substitution lemma.  The others follow directly from the
        induction hypotheses. *)

Lemma preservation_result : preservation_statement.
Proof.
  introv Typ. gen t'.
  induction Typ; intros t' Red; inversions Red.
  inversions Typ1. pick_fresh x. rewrite* (@subst_intro x).
   apply_empty* typing_subst.
  apply* typing_app.
  apply* typing_app.
Qed.

(** The proof of progress appears below.
    Proof sketch: By induction on the typing derivation for [t].

      - [typing_var] case: Can't happen; the empty environment
        doesn't bind anything.

      - [typing_abs] case: Abstractions are values.

      - [typing_app] case: Applications reduce.  The result follows
        from an exhaustive case analysis on whether the two components
        of the application step or are values and the fact that a
        value must be an abstraction. *)

Lemma progress_result : progress_statement.
Proof.
  introv Typ. gen_eq E: (empty:ctx). lets Typ': Typ.
  induction Typ; intros; subst.
  false* binds_empty_inv. 
  left*.
  right. destruct~ IHTyp1 as [Val1 | [t1' Red1]].
    destruct~ IHTyp2 as [Val2 | [t2' Red2]].
      inversions Typ1; inversions Val1. exists* (t0 ^^ t2).
      exists* (trm_app t1 t2'). 
    exists* (trm_app t1' t2).
Qed.

End AxiomatizedVersion.



(* ********************************************************************** *)
(* ********************************************************************** *)
(* ********************************************************************** *)
(** * Removing all axioms *)

Module CompleteVersion.

(** At this point we come back to the infrastructure part and try
    to prove all remaining axioms and meta-axioms. We will need to
    re-check all our proofs. This is usually done in-place in the
    file, however in this tutorial we have copy-pasted all the proofs.
*)

(* ********************************************************************** *)
(** ** Proving the two axioms *)

(** We first set up four lemmas, and then we can prove our two axioms. *)

(** The first lemma is a technical auxiliary lemma which do not 
    want and do not need to read. *)

Lemma open_rec_term_core :forall t j v i u, i <> j ->
  {j ~> v}t = {i ~> u}({j ~> v}t) -> t = {i ~> u}t.
Proof.
  induction t; introv Neq Equ; simpls; inversion* Equ; fequals*.
  case_nat*. case_nat*.
Qed.

(** Substitution on indices is identity on well-formed terms. *)

Lemma open_rec_term : forall t u,
  term t -> forall k, t = {k ~> u}t.
Proof.
  induction 1; intros; simpl; fequals*. unfolds open.
  pick_fresh x. apply* (@open_rec_term_core t1 0 (trm_fvar x)).
Qed.

(** Substitution for a fresh name is identity. *)

Lemma subst_fresh : forall x t u, 
  x \notin fv t ->  [x ~> u] t = t.
Proof. intros. induction t; simpls; fequals*. case_var*. Qed.

(** Substitution distributes on the open operation. *)

Lemma subst_open : forall x u t1 t2, term u -> 
  [x ~> u] (t1 ^^ t2) = ([x ~> u]t1) ^^ ([x ~> u]t2).
Proof.
  intros. unfold open. generalize 0.
  induction t1; intros; simpl; fequals*.
  case_nat*. case_var*. apply* open_rec_term.
Qed.

(** Substitution and open_var for distinct names commute. *)

Lemma subst_open_var : forall x y u t, y <> x -> term u ->
  ([x ~> u]t) ^ y = [x ~> u] (t ^ y).
Proof. introv Neq Wu. rewrite* subst_open. simpl. case_var*. Qed.

(** Opening up an abstraction of body [t] with a term [u] is the same as opening
  up the abstraction with a fresh name [x] and then substituting [u] for [x]. *)

Lemma subst_intro : forall x t u, 
  x \notin (fv t) -> term u ->
  t ^^ u = [x ~> u](t ^ x).
Proof.
  introv Fr Wu. rewrite* subst_open.
  rewrite* subst_fresh. simpl. case_var*.
Qed.


(* ********************************************************************** *)
(** ** Preservation of local closure *)

(** The goal of this section is to set up the appropriate lemmas 
    for proving goals of the form [term t]. First, we defined a
    predicate capturing that a term [t] is the body of a locally
    closed abstraction. *)

Definition body t :=
  exists L, forall x, x \notin L -> term (t ^ x).

(** We then show how to introduce and eliminate [body t]. *)

Lemma term_abs_to_body : forall t1, 
  term (trm_abs t1) -> body t1.
Proof. intros. unfold body. inversion* H. Qed.

Lemma body_to_term_abs : forall t1, 
  body t1 -> term (trm_abs t1).
Proof. intros. inversion* H. Qed.

Hint Resolve term_abs_to_body body_to_term_abs.

(** We prove that terms are stable by substitution *)

Lemma subst_term : forall t z u,
  term u -> term t -> term ([z ~> u]t).
Proof.
  induction 2; simpls*.
  case_var*.
  apply_fresh term_abs. rewrite* subst_open_var.
Qed.

Hint Resolve subst_term.

(** We prove that opening a body with a term gives a term *)

Lemma open_term : forall t u,
  body t -> term u -> term (t ^^ u).
Proof.
  intros. destruct H. pick_fresh y. rewrite* (@subst_intro y).
Qed.

Hint Resolve open_term.


(* ********************************************************************** *)
(** ** Regularity of relations *)

(** The last step to set up the infrastructure consists in proving
    that relations are "regular". For example, a typing relation can 
    hold only if the environment has no duplicated keys and the term 
    involved is locally-closed. *)

Lemma typing_regular : forall E e T,
  typing E e T -> ok E /\ term e.
Proof.
  split; induction* H. 
  pick_fresh y. forwards~ : (H0 y).
Qed.

(** Similarly, the value predicate only holds on locally-closed terms. *)

Lemma value_regular : forall e,
  value e -> term e.
Proof. induction 1; auto*. Qed.

(** A reduction relation only holds on pairs of locally-closed terms. *)

Lemma red_regular : forall e e',
  red e e' -> term e /\ term e'.
Proof. induction 1; auto* value_regular. Qed.

(** The strength of automation comes from the following custom hints.
    They are easy to set up because the follow a very regular pattern.
    These hints indicate that to prove a goal of the form [ok E],
    it suffices to find in the goal an hypothesis of the form
    [typing E t T] and to exploit the regularity lemma [typing_regular]
    to prove the goal. Similarly, properties of the form [term t]
    can be extracted out of typing or reduction or value judgments.
*)

Hint Extern 1 (ok ?E) =>
  match goal with
  | H: typing E _ _ |- _ => apply (proj1 (typing_regular H))
  end.

Hint Extern 1 (term ?t) =>
  match goal with
  | H: typing _ t _ |- _ => apply (proj2 (typing_regular H))
  | H: red t _ |- _ => apply (proj1 (red_regular H))
  | H: red _ t |- _ => apply (proj2 (red_regular H))
  | H: value t |- _ => apply (value_regular H)
  end.

(* ********************************************************************** *)
(** ** Checking that the main proofs still type-check *)

(** We conclude our development by showing that, with the appropriate
    hints being set up, we can recompile our proofs without changing 
    any single character in them.
*)

Lemma typing_weaken : forall G E F t T,
   (E & G) |= t ~: T -> 
   ok (E & F & G) ->
   (E & F & G) |= t ~: T.
Proof.
  introv Typ. gen_eq H: (E & G). gen G.
  induction Typ; intros G EQ Ok; subst.
  apply* typing_var. apply* binds_weaken.
  apply_fresh* typing_abs as y. apply_ih_bind* H0.
  apply* typing_app.
Qed.

Lemma typing_subst : forall F E t T z u U,
  (E & z ~ U & F) |= t ~: T ->
  E |= u ~: U ->
  (E & F) |= [z ~> u]t ~: T.
Proof.
  introv Typt Typu. gen_eq G: (E & z ~ U & F). gen F.
  induction Typt; intros G Equ; subst; simpl subst.
  case_var.
    binds_get H0. apply_empty* typing_weaken.
    binds_cases H0; apply* typing_var.
  apply_fresh typing_abs as y.
   rewrite* subst_open_var. apply_ih_bind* H0.
  apply* typing_app.
Qed.

Lemma preservation_result : preservation_statement.
Proof.
  introv Typ. gen t'.
  induction Typ; intros t' Red; inversions Red.
  inversions Typ1. pick_fresh x. rewrite* (@subst_intro x).
   apply_empty* typing_subst.
  apply* typing_app.
  apply* typing_app.
Qed.

Lemma progress_result : progress_statement.
Proof.
  introv Typ. gen_eq E: (empty:ctx). lets Typ': Typ.
  induction Typ; intros; subst.
  false* binds_empty_inv. 
  left*.
  right. destruct~ IHTyp1 as [Val1 | [t1' Red1]].
    destruct~ IHTyp2 as [Val2 | [t2' Red2]].
      inversions Typ1; inversions Val1. exists* (t0 ^^ t2).
      exists* (trm_app t1 t2'). 
    exists* (trm_app t1' t2).
Qed.


End CompleteVersion.

