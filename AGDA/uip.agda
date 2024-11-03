{-# OPTIONS --without-K #-}

open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.Unit

-- Axiome d'univalence
-- Définition de l'équivalence
record _≃_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
    to-from : (x : A) → from (to x) ≡ x
    from-to : (y : B) → to (from y) ≡ y

-- Axiome d'univalence
postulate
  univalence : {A B : Set} → (A ≃ B) → (A ≡ B)

-- Unicité des preuves d'identité (UIP)
UIP : Set₁
UIP = {A : Set} {x y : A} (p q : x ≡ y) → p ≡ q

URP : Set₁
URP = {A : Set} {x : A} (p : x ≡ x) → p ≡ refl

-- Axiome K
K : Set₁
K = {A : Set} {x : A} (P : (x ≡ x) → Set) → P refl → (p : x ≡ x) → P p

J : {A : Set} {x : A} (P : (y : A) → x ≡ y → Set) → P x refl → {y : A} (p : x ≡ y) → P y p
J P pr {x} refl = pr

K-implies-URP : K → URP
K-implies-URP k p =  k (λ p → p ≡ refl) refl p 

--Exemple d'utilisation de l'axiome d'univalence
open import Agda.Builtin.Bool

-- Définissons une équivalence entre Bool et un type à deux éléments
data Two : Set where
  zero : Two
  one  : Two

boolToTwo : Bool → Two
boolToTwo true  = one
boolToTwo false = zero

twoToBool : Two → Bool
twoToBool one  = true
twoToBool zero = false

boolTwoEquiv : Bool ≃ Two
boolTwoEquiv = record
  { to = boolToTwo
  ; from = twoToBool
  ; to-from = λ { true → refl ; false → refl }
  ; from-to = λ { one → refl ; zero → refl }  
  }

-- Utilisation de l'axiome d'univalence pour montrer que Bool ≡ Two
boolEqTwo : Bool ≡ Two
boolEqTwo = univalence boolTwoEquiv

th-ext : (λ x → 0 + x) ≡ (λ x → x)
th-ext = refl

data ⊥ : Set where

-- type egalité extension pour le type Two
data Two≡ :  Two → Two → Set where
  Two≡z zero zero : Two≡
  Two≡o one one   : Two≡   

th-ext-Two : {x y : Two} → x ≡ y → (Two≡ x y)
th-ext-Two refl =  {!   !}