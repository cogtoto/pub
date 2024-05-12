-- cantor.agda
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality
open import Agda.Builtin.Unit

data False : Set where

data Dec (A : Set) : Set where
  yes : A → Dec A
  no : (A → False) → Dec A

negb : Bool → Bool
negb true = false
negb false = true

non : Set → Set 
non A = A → False

data Σ (A : Set) (B : A → Set) : Set where
 _,_ : (a : A) → B a → Σ A B

surjective : {Y : Set} (f : Nat → Y) → Set
surjective {Y} f =  (y : Y) → (Σ Nat (λ x →  ((f x) ≡ y)))

-- un peu d'entrainement pour manipuler cette fonction surjective
data Unelt : Set where
  tt : Unelt

tuto : Nat → Bool 
tuto _ = false

th6 : Σ Nat (λ x → tuto x ≡ true) → False
th6 (a ,  () )

tuto_notsurjective : ( (y : Bool) → (Σ Nat (λ x  → ((tuto x) ≡ y))) ) → False
tuto_notsurjective b = th6 (b true)

g : Bool → Bool
g x = negb x

negbprop : (b : Bool) →  ( g b  ≡ b ) → False
negbprop true = λ ()
negbprop false =  λ ()

diagneg : (f : Nat → Nat → Bool) → (x : Nat) →  g (f x x) ≡ (f x x) → False
diagneg f x = negbprop (f x x)   

th41 : zero ≡ 2 → False
th41 ()

th42 : ((x : Nat) → x ≡ 2) → False
th42 f =  th41 (f zero)

diagneg2 : ((f : Nat → Nat → Bool) → (h : Nat → Bool) → (x : Nat) →  h x ≡ (f x x))  → False
diagneg2 C  =  negbprop (C f h1 zero) 
           where h1 : Nat → Bool
                 h1 x2 = g (f x2 x2)


-- lemme : ((h : Nat → Bool) → Σ Nat (λ x → (g (fce x x)) ≡ (h x)))  → False
-- lemme hh =  {!   !}

-- lemme2 : (f : Nat → Nat → Bool) → ((h : Nat → Bool) → Σ Nat (λ x → (g (f x x)) ≡ (h x)))  → False
-- lemme2 f  = {! ?  !}

-- cantor :  (Σ (Nat → Nat → Bool)  (λ f → 
                                   --( (y : Nat → Bool) → (Σ Nat (λ x →  ((f x) ≡ y ))) )
           -- )) → False

-- cantor (f , y) =  {! y  !}

-- cantor :  (Σ (Nat → Nat → Bool)  (λ f → surjective f)) → False