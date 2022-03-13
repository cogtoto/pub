(** miniML *)
Require Import String.
Open Scope string_scope.
Require Import List.
Open Scope list_scope.
Import ListNotations. 

Section miniML.
(* représentation des termes *)

Inductive terme :=
| Vrai : terme
| Faux : terme
| Var : string -> terme
| Nb : nat -> terme
| Plus : terme -> terme -> terme
| Moins : terme -> terme -> terme
| Et : terme -> terme -> terme 
| Si : terme -> terme -> terme -> terme
| Lambda: string -> terme -> terme
| Fermeture : string -> terme -> list (string*terme) -> terme
| App : terme -> terme -> terme (* App M N *)
| Let : string -> terme -> terme -> terme (* let x N M *)
| Letrec : string -> terme -> terme -> terme (* letrec x N M *)
.

Coercion Var : string >-> terme.
Coercion Nb : nat >-> terme.

Inductive valeur : terme -> Prop := 
| Nb_v : forall n:nat, valeur (Nb n)
| Vrai_v : valeur Vrai
| Faux_va:  valeur Faux
.

(* représentation de l'environnement  *)
Definition env := list (string * terme) .
Definition env0 : env := [("z",Nb 1); ("y", Nb 2); ("v", Vrai)].

Fixpoint lookup (ident:string) (e:env):terme :=
    match e with
    | [] => Faux
    | (x,y)::reste => if x =? ident then y else lookup ident reste
    end.

(** la sémantique *)
Inductive sem (e:env): terme -> terme -> Prop  :=
| r_vrai : sem e Vrai Vrai
| r_faux : sem e Faux Faux
| r_nb : forall n:nat, sem e (Nb n) (Nb n)
| r_var : forall x:string, sem e (Var x) (lookup x e)

| r1_et : forall (t1 t2:terme) , sem e t1 Faux  -> sem e (Et t1 t2) Faux
| r2_et : forall (t1 t2 v2:terme),
    sem e t1 Vrai  ->
    sem e t2 v2 -> 
    valeur v2 ->
    sem e (Et t1 t2) v2

| r_plus : forall (t1 t2:terme) (n1 n2 n3:nat) ,
    sem e t1 (Nb n1) -> 
    sem e t2 (Nb n2) -> 
    n3 = n2 + n1 ->
    sem e (Plus t1 t2) (Nb n3)

| r_moins : forall (t1 t2:terme) (n1 n2 n3:nat) ,
    sem e t1 (Nb n1) -> 
    sem e t2 (Nb n2) -> 
    n3 = n1 - n2 ->
    sem e (Moins t1 t2) (Nb n3)

| r_si_vrai : forall (t1 t2 v2 t3:terme), sem e t1 Vrai -> sem e t2 v2 ->
              valeur v2 -> sem e (Si t1 t2 t3) v2
| r_si_faux : forall (t1 t2 t3 v3:terme), sem e t1 Faux -> sem e t3 v3 ->
              valeur v3 -> sem e (Si t1 t2 t3) v3

(* lambda *)
| r_lambda : forall (x:string) (corps:terme), sem e (Lambda x corps) (Fermeture x corps e)

(* application  App M N *)
| r_app : forall (M M' N v v' :terme) (x:string) (e':env),
    sem e M (Fermeture x M' e') -> 
    sem e N v ->
    sem  ([(x,v)] ++ e') M' v' ->
    sem e (App M N) v'

(* let x N M *)
| r_let : forall (N M v v' :terme) (x:string) ,
    sem e N v  ->
    sem ([(x,v)] ++ e) M v' ->
    sem e (Let x N M) v'

(* letrec x N M *)
| r_letrec : forall (N M v v' :terme) (x:string) ,
    sem ([(x,v)] ++ e) N v  ->
    sem ([(x,v)] ++ e) M v' ->
    sem e (Letrec x N M) v'
.

Lemma  test1 : sem env0 (Var "z") (Nb 1) .
apply r_var. Qed.

Hint Constructors sem valeur .

Lemma test3 : sem env0 (Et Vrai Faux) Faux .
apply r2_et. 
auto. auto. auto.
Qed.

Lemma test2 : sem env0 (Et Vrai (Et (Var "v") Faux)) Faux.
apply r2_et.
auto. apply r2_et.
apply r_var. auto. auto. auto.
Qed.

Lemma test4 : sem env0 (Et Vrai (Et (Var "v") Vrai)) Vrai.
apply r2_et. repeat auto. apply r2_et. apply r_var. auto. auto. auto.
Qed.

Lemma test5 : sem env0 (Plus (Nb 2) (Nb 5)) (Nb 7).
simpl.
eapply r_plus.  auto. auto. simpl. auto.
Qed.

Lemma test6 : sem env0 (Si Vrai (Nb 3) (Nb 5)) (Nb 3).
apply r_si_vrai. auto. auto. auto.
Qed.

Definition id : terme := Lambda "x" (Var "x").
Lemma t11 : sem env0 (App id (Nb 5)) (Nb 5).
unfold id.
eapply r_app.
apply r_lambda.
apply r_nb.
simpl.
apply r_var.
Qed.

Definition succ : terme := Lambda "x" (Plus (Var "x") (Nb 1)).

Lemma t12 : sem (("x", Nb 5)::env0) (Var "x") (Nb 5).
apply r_var.
Qed.

Lemma t10 : sem env0 (App succ (Nb 5)) (Nb 6).
unfold succ.
eapply r_app.
eapply r_lambda.
eapply r_nb.
simpl.
eapply r_plus.
unfold env0.

assert (sem [("x", Nb 5); ("z", Nb 1); ("y", Nb 2); ("v", Vrai)] (Var "x") (Nb 5)).
 apply r_var.
 eauto.  
apply r_nb.
simpl. 
auto.
Qed.

Lemma t13 : sem env0 (Moins (Nb 6) (Nb 5)) (Nb 1).
eapply r_moins.
apply r_nb.
apply r_nb.
simpl.
reflexivity.
Qed.

Lemma t14 : sem env0 (Let "x" (Nb 5) (Var "x")) (Nb 5).
eapply r_let.
apply r_nb.
apply r_var.
Qed.

Lemma t15: sem env0 (Letrec "mon_plus" (Lambda "x" (Plus (Var "x") (Nb 1))) (App (Var "mon_plus") (Nb 5))) (Nb 6).
eapply r_letrec.
eapply r_lambda.
Admitted.


Fixpoint mon_plus (n m:nat) {struct n}  : nat :=
match n with
| 0 => m 
| S n => S (mon_plus n m)
end.

Compute mon_plus 3 4.

Compute (fun x=> x+1) 3.
Compute (let x := 3 in x+1).

Notation "e |= A :~ B" := (sem e A B) (at level 69).

Lemma t16 : env0 |= "y" :~ 2.
unfold env0.
apply r_var.
Qed.

