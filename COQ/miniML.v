(** miniML *)
Require Import String.
Open Scope string_scope.
Require Import List.
Open Scope list_scope.
Import ListNotations. 

Section miniML.
(** représentation des termes *)

Inductive terme :=
| Vrai : terme
| Faux : terme
| Var : string -> terme
| Nb : nat -> terme
| Plus : terme -> terme -> terme
| Et : terme -> terme -> terme 
| Si : terme -> terme -> terme -> terme
| Tete : list terme -> terme
| Liste : list terme -> terme
| Cons : terme -> list terme -> terme
| Reste : list terme ->  terme
| Lambda: string -> terme -> terme
| Fermeture : string -> terme -> list (string*terme) -> terme
| App : terme -> terme -> terme
.

Inductive valeur : terme -> Prop := 
| Nb_v : forall n:nat, valeur (Nb n)
| Vrai_v : valeur Vrai
| Faux_va:  valeur Faux
.

(** représentation de l'environnement [\Gamma] *)
Definition env := list (string * terme) .

Definition env0 : env := [("x",Nb 1); ("y", Nb 2); ("v", Vrai)].
Print env0.

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
| r2_et : forall (t1 t2 v2:terme) , sem e t1 Vrai  -> sem e t2 v2 -> valeur v2 -> sem e (Et t1 t2) v2
| r_plus : forall (t1 t2:terme) (n1 n2:nat) , sem e t1 (Nb n1) -> sem e t2 (Nb n2) -> sem e (Plus t1 t2) (Nb (n1+n2))
| r_si_vrai : forall (t1 t2 v2 t3:terme), sem e t1 Vrai -> sem e t2 v2 -> valeur v2 -> sem e (Si t1 t2 t3) v2
| r_si_faux : forall (t1 t2 t3 v3:terme), sem e t1 Faux -> sem e t3 v3 -> valeur v3 -> sem e (Si t1 t2 t3) v3
(* fonctions sur liste*)
| r_cons : forall (v1:terme) (l2: list terme), valeur v1 -> sem e (Cons v1 l2) (Liste (v1::l2))
(* lambda expression *)
| r_lambda : forall (x:string) (corps:terme), sem e (Lambda x corps) (Fermeture x corps e)
(* application *)
| r_app : forall (f corps t1 v1 w:terme) (x:string) (env1:env),
    sem e f (Fermeture x corps env1) -> 
    sem e t1 v1 ->
    sem  ([(x,v1)] ++ env1) corps w ->
    sem e (App f t1) w
.


Lemma  test1 : sem env0 (Var "x") (Nb 1) .
apply r_var. Qed.

Hint Constructors sem valeur .

Lemma test3 : sem env0 (Et Vrai Faux) Faux .
apply r2_et. 
auto. auto. auto.
Qed.

Lemma test2 : sem env0 (Et Vrai (Et (Var "z") Vrai)) Faux.
apply r2_et.
auto. apply r1_et.
apply r_var. auto.
Qed.

Lemma test4 : sem env0 (Et Vrai (Et (Var "v") Vrai)) Vrai.
apply r2_et. repeat auto. apply r2_et. apply r_var. auto. auto. auto.
Qed.

Lemma test5 : sem env0 (Plus (Nb 2) (Nb 5)) (Nb (2+5)).
apply r_plus.  auto. auto.
Qed.

Lemma test6 : sem env0 (Si Vrai (Nb 3) (Nb 5)) (Nb 3).
apply r_si_vrai. auto. auto. auto.
Qed.

Lemma t7 : sem env0 (Cons (Nb 5) [(Nb 6); (Nb 7)]) (Liste [(Nb 5); (Nb 6); (Nb 7)]).
auto.
Qed.

Lemma t8 : exists t:terme, sem env0 (Cons (Nb 5) [(Nb 6); (Nb 7)]) t.
eauto .
Qed.

Lemma t9 : {t:terme| sem env0 (Cons (Nb 5) [(Nb 6); (Nb 7)]) t}.
eauto.
Qed.

Print t9.

Definition succ : terme := Lambda "x" (Plus (Var "x") (Nb 1)).

Lemma t10 : sem env0 (App succ (Nb 5)) (Nb 6).
unfold succ.
eapply r_app.
eapply r_lambda.
eapply r_nb.
simpl.
assert (sem [("x", Nb 1); ("y", Nb 2); ("v", Vrai); ("x", Nb 5)] (Var "x") (Nb 1)).
eapply r_var.
apply (r_plus 

(*  r_plus : forall (t1 t2:terme) (n1 n2:nat) , sem e t1 (Nb n1) -> sem e t2 (Nb n2) -> sem e (Plus t1 t2) (Nb (n1+n2)) *)