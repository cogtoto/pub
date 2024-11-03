-- lambda calculus in agda
open import Agda.Builtin.String 
open import Agda.Builtin.Equality
open import Agda.Builtin.Nat 
open import Agda.Builtin.Sum 
open import Agda.Builtin.Bool 
-- open import Relation.Binary
open import Relation.Binary.PropositionalEquality
-- open import Relation.Nullary.Decidable -- Ajout de l'importation correcte
open import Data.Sum using (_⊎_; inj₁; inj₂)


Dec : Set → Set
Dec A = A  A

Decidable : {A : Set} (R : A → A → Set) → Set
Decidable {A} R = (x y : A) → Dec (R x y)


data ⊥ : Set where

⊥-elim : ∀ {w} {Whatever : Set w} → ⊥ → Whatever
⊥-elim ()


if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

infix 7 lam_
infixl 8 _∘_
infix  9 v

data Terme : Set where
 v : Nat → Terme
 lam_ : Terme → Terme
 _∘_ : Terme → Terme → Terme

omega = lam  (v 0 ∘ v 0)
Omega = omega ∘ omega
K = lam (lam (v 1))
S = lam (lam (lam ((((v 2) ∘ (v 0)) ∘ ((v 1) ∘ (v 0)))) ))
I = lam (v 0)
SKI = (S ∘ K) ∘ I

-- 0 := λf.λx.x
-- 1 := λf.λx.f x

-- 2 := λf.λx.f (f x)

-- succ := λn.λf.λx.f (n f x)
↑ : Nat → Nat → Nat
↑ zero y = suc y
↑ (suc x) zero = zero
↑ (suc x) (suc y) = suc (↑ x y)

↓ : (x y : Nat) → (x ≡ y → ⊥) → Nat
↓ zero zero ¬p = ⊥-elim (¬p refl)
↓ zero (suc y) ¬p = y
↓ (suc x) zero ¬p = zero
↓ (suc x) (suc y) ¬p = suc (↓ x y (λ p → ¬p (cong suc p)))

wk : Nat → Terme → Terme
wk x (v y) = v (↑ x y)
wk x (t ∘ t') = wk x t ∘ wk x t'
wk x (lam t) = lam (wk (suc x) t)


infix 10 _[_/_]

_[_/_] : Terme → Terme → Nat → Terme
v y [ u / x ] with x ≟  y
(v y [ u / _ ]) | true _ = u
(v y [ u / x ]) | false ¬p = v (↓ x y ¬p)
(t ∘ t') [ u / x ] = (t [ u / x ]) ∘ (t' [ u / x ])
(lam t) [ u / x ] = lam (t [ wk 0 u / ↑ 0 x ])


reduc : Terme → Terme
reduc (v x) = v x 
reduc (lam t) = lam (reduc t)
reduc (t₁ ∘ t₂) with (reduc t₁)
...             | (lam l) = l [ (reduc t₂) / 0 ]
...             | t       = t ∘ (reduc t₂)

normalize : Nat → Terme → Terme
normalize 0 t = t 
normalize (suc fuel) t = normalize fuel  (reduc t)

test = lam ((lam (v 1)) ∘ (lam (v 0)))
test2 = lam ((lam (lam (v 1))) ∘ (v 0))
-- t3 := (λu.λv.u x) y → λv.y x
-- t3 := (λ.λ.1 2) 1 → λ.(2 1)
t3 = lam (lam (v 1 ∘ v 2)) ∘ v 1

-- 2  λ f . λx. f ( f x)
deux = lam lam (v 1 ∘ (v 1 ∘ v 0))
-- SUCC ≜ λn. λ f . λx. f (n f x)
-- succ = lam ((v 0) ∘ lam (v 1 ∘ v 0))
-- succ = lam (lam (lam (v 1 ∘ (v 2  ∘ v 1 ∘ v 0))))

add = lam lam lam lam (v 3 ∘ v 1 ∘ (v 2 ∘ v 1 ∘ v 0))

p = normalize 1000 (add ∘ deux) ∘ deux
