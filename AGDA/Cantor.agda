-- first proof in agda , le théorème de cantor
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Equality
open import Agda.Builtin.Sigma

data False : Set where

cong-app : ∀ {A : Set } {B : A → Set } {f g : (x : A) → B x} →
           f ≡ g → (x : A) → f x ≡ g x
cong-app refl x = refl

surj : {A B : Set} → (f : A → B) → Set
surj  {A} {B} f =  (b : B) → Σ A (λ a → (f a) ≡ b)

g : Bool → Bool
g false = true
g true = false

lemme : (a : Bool) → a ≡ g a → False
lemme true = λ ()  
lemme false = λ ()  

cantor : (f : Nat → Nat → Bool) → surj f → False
cantor f sur = fxx≢hx x (cong-app hyp x) 
  where 
  fxx≢hx : (x : Nat) → f x x ≡ g (f x x) → False
  fxx≢hx x = lemme (f x x)
  h : Nat → Bool
  h x = g (f x x) -- h la diagonalisation négative de la mort
  x : Nat
  x = fst (sur h)
  hyp : f x ≡ h     -- on applique l'hypothèse de la surjection sur h, donc il existe un x tq f x = h
  hyp = snd (sur h)
  
