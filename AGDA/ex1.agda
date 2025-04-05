--  agda

open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.Bool
open import Agda.Builtin.List

data Deux : Set where
  un : Deux
  deux : Deux

rec : {A : Set}  (C : Deux → Set) → C un → C deux → (x : Deux) → C x 
rec {A} C c1 c2 un = c1
rec {A} C c1 c2 deux = c2

data Coproduit (A : Set) (B : Set) : Set where
  gauche : A → Coproduit A B
  droite : B → Coproduit A B

ind-coproduit : {A B : Set} (C : (Coproduit A B) → Set) → 
                            ((a : A) → C (gauche a))  → 
                            ((b : B) → C (droite b))  → 
                            ((x : Coproduit A B) → C x)
ind-coproduit c ga gb (gauche x) = ga x
ind-coproduit c ga gb (droite x) = gb x

th1 : (x : Deux) → Coproduit (x ≡ un) (x ≡ deux) 
th1 un = gauche refl 
th1 deux = droite refl

data faux : Set where

rec-faux : (C : Set) → faux → C 
rec-faux C ()

ind-faux : (C : faux → Set) → (x : faux) → C x  
ind-faux C ()
