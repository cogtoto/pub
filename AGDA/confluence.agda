open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Equality
open import Agda.Builtin.Sigma

record _×_ (A B : Set) : Set where
  constructor ⟨_,_⟩
  field
    proj₁ : A
    proj₂ : B
open _×_

if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

data lambda : Set where
  Ref : Nat → lambda
  Abs : lambda → lambda
  App : lambda → lambda → lambda

lift-rec : Nat → lambda → Nat → lambda
lift-rec n (Ref i) k = if (i < k) then (Ref i) else Ref (n + i)
lift-rec n (Abs M) k = Abs (lift-rec n M (k + 1))
lift-rec n (App M N) k = App (lift-rec  n M k) (lift-rec n N k)

lift : Nat → lambda → lambda
lift n N = lift-rec n N 0

subst-rec : lambda → lambda → Nat → lambda
subst-rec N (Ref i) k = if (k < i) then (Ref (i - 1)) else (if (k == i) then (lift k N) else (Ref i)) 
subst-rec N (Abs P) k = Abs (subst-rec N P (k + 1))
subst-rec N (App P Q) k = App (subst-rec N P k) (subst-rec N Q k)

subst : lambda → lambda → lambda
subst N M = subst-rec N M 0 

K = Abs (Abs (App (Ref 0) (Ref 1))) -- K = λ x . λ y . x
I = Abs (Ref 0) -- I ≡ λ x . x 
KI = App K I 
res = subst K I 

-- one step β-reduction
data red1 : lambda → lambda → Set where
  beta : (M N : lambda) → (red1 (App (Abs M) N) (subst N M)) 
  abs-red : (M N : lambda) → (red1 M N) → (red1 (Abs M) (Abs N))
  app-red-l : (M1 N1 : lambda) → (red1 M1 N1) → (M2 : lambda) → (red1 (App M1 M2) (App N1 M2)) 
  app-red-r : (M2 N2 : lambda) → (red1 M2 N2) → (M1 : lambda) → (red1 (App M1 M2) (App M1 N2)) 

-- β-reduction as the transitive closure of red1
data red : lambda → lambda → Set where
  one-step-red : (M N : lambda) → (red1 M N) → (red M N)
  refl-red : (M : lambda) → red M M
  trans-red : (M N P : lambda) → (red M N) → (red N P) → (red M P) 

  
confluence : (A : Set) → (R : (A → A → Set)) → Set
confluence A R =  (x y : A) → (R x y) → (z : A) → (R x z) → Σ A (λ u → (R y u) × (R z u))

data conv1 : lambda →  lambda → Set where
  red1_conv : (M N : lambda) → red1 M N → conv1 M N
  exp1_conv : (M N : lambda) → red1 N M → conv1 M N

data conv : lambda → lambda → Set where
  one-step-conv  : (M N : lambda) → conv1 M N → conv M N
  ref-conv : (M : lambda) → conv M M
  trans-conv : (M N P : lambda) → conv M N → conv N P → conv M P

-- Lemme conv est symétrique
sym-conv : (M N : lambda) → conv M N → conv N M
sym-conv (Ref x) (Ref x₁) p = {! one-step-conv   !}
sym-conv (Ref x) (Abs N) p = {!   !}
sym-conv (Ref x) (App N N₁) p = {!   !}
sym-conv (Abs M) N p = {!   !}
sym-conv (App M M₁) N p = {!   !}

-- Church Rosser
-- Church_Rosser : (M N : lambda) → (conv M N) → Σ lambda (λ P → (red M P) × (red N P))

