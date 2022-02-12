(** miniML *)
Require Import String.
Open Scope string_scope.

(** représentation des termes *)

Inductive terme :=
| Vrai : terme
| Faux : terme
| Var : string -> terme
| Nb : nat -> terme
| Plus : terme -> terme -> terme
| Et : terme -> terme -> terme .

Inductive valeur : terme -> Prop := 
| Nb_v : forall n:nat, valeur (Nb n)
| Vrai_v : valeur Vrai
| Faux_va:  valeur Faux
.

(** représentation de l'environnement [\Gamma] *)

Definition env_init (ident:string):terme :=
    match ident with
    | "x" => Nb 1
    | "y" => Nb 2
    | "v" => Vrai 
    | _ => Faux
    end.

    (** la sémantique *)
Inductive sem: terme -> terme -> Prop  :=
| r_vrai : sem Vrai Vrai
| r_faux : sem Faux Faux
| r_nb : forall n:nat, sem (Nb n) (Nb n)
| r_var : forall x:string, sem (Var x) (env_init x)
| r1_et : forall (e1 e2:terme) , sem e1 Faux  -> sem (Et e1 e2) Faux
| r2_et : forall (e1 e2 v2:terme) , sem e1 Vrai  -> sem e2 v2 -> valeur v2 -> sem (Et e1 e2) v2
| r_plus : forall (e1 e2:terme) (n1 n2:nat) , sem e1 (Nb n1) -> sem e2 (Nb n2) -> sem (Plus e1 e2) (Nb (n1+n2))
.

(** le type des expressions *) 


Lemma  test1 : sem (Var "x") (Nb 1) .
apply r_var. Qed.

Hint Constructors sem valeur .

Lemma test3 : sem (Et Vrai Faux) Faux .
apply r2_et. 
auto. auto. auto.
Qed.

Lemma test2 : sem (Et Vrai (Et (Var "z") Vrai)) Faux.
apply r2_et.
auto. apply r1_et.
apply r_var. auto.
Qed.

Lemma test4 : sem (Et Vrai (Et (Var "v") Vrai)) Vrai.
apply r2_et. repeat auto. apply r2_et. apply r_var. auto. auto. auto.
Qed.

Lemma test5 : sem (Plus (Nb 2) (Nb 5)) (Nb (2+5)).
apply r_plus.  auto. auto.
Qed.
