(* sémantique dénotationnelle du lambda-calcul *)
Require Import String.
Require Import List .
Import ListNotations.

Inductive terme :=
| var : string -> terme
| app : terme -> terme -> terme
| abs : string -> terme -> terme .

Inductive value :=
| v_var : terme -> value
| v_app : value -> value -> value
| v_abs : value -> value
| undefined.

Definition env := list (string*value).

Fixpoint rho (a:string) (e:env) : value :=
	match e with
	  | [] => undefined
	  | (i,va) :: m => if (eqb i a) then va else rho a m
	end.

Fixpoint valuation (pi:terme) (e:env) : value :=
  match pi with
  | var s => rho s e
  | app pi1 pi2 => v_app (valuation pi1 e) (valuation pi2 e)
  | abs x pi1 => v_abs (fun (v:value) => (valuation pi ((x,v)::e)))
  end.
