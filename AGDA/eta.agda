-- typed lambda calculus 

open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.Bool
open import Agda.Builtin.List

infix 0 if_then_else_

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then t else f = t
if false then t else f = f

append : {A : Set} → List A → List A → List A
append [] ys = ys
append (x ∷ xs) ys = x ∷ (append xs ys)

data type : Set where
  typ_var : String → type
  typ_arrow : type → type → type

data terme : Set where
  bvar : Nat → terme
  fvar : String → terme
  abs : terme → terme
  app : terme → terme → terme

-- t1 = λ x. λ y. (y x)
t1 : terme
t1 = abs (abs (app (bvar 0) (bvar 1)))

-- opening
-- L’opening remplace un indice par un terme. Cela correspond à la substitution d’une variable
-- liée, telle qu’appliquée lors de la β-réduction.

open-rec : Nat → terme → terme → terme
open-rec k u (bvar i) = if (i == k) then u else (bvar i)
open-rec k u (fvar x) = fvar x
open-rec k u (abs t) = abs (open-rec (suc k) u t)
open-rec k u (app t1 t2) = app (open-rec k u t1) (open-rec k u t2)

-- open
op : terme → terme → terme
op t u = open-rec 0 u t

-- op (λ (1 0) 0) y ≡  λ (y 0) y
demo-open : (op (app (abs (app (bvar 1) (bvar 0))) (bvar 0)) (fvar "y")) ≡ (app (abs (app (fvar "y") (bvar 0))) (fvar "y"))
demo-open =  refl

-- La sémantique avec appel par valeur

data valeur : terme → Set where
  v_abs : (t : terme) → valeur (abs t)
  v_nat : (n : Nat) → valeur (bvar n)
  v_var : (x : String) → valeur (fvar x)

data _↦_ : terme → terme → Set where
  red-beta : (t1 t2 : terme) → valeur t2 → (app (abs t1) t2) ↦ (op t1 t2)
  red-app-1 : (t1 t1' t2 : terme) → t1 ↦ t1' → (app t1 t2) ↦ (app t1' t2)
  red-app-2 : (t1 t2 t2' : terme) → t2 ↦ t2' → (app t1 t2) ↦ (app t1 t2')

-- free variables
fv : terme → List String
fv (bvar n) = []
fv (fvar x) =  x ∷ []
fv (abs t) = fv t
fv (app t1 t2) = append (fv t1) (fv t2)

