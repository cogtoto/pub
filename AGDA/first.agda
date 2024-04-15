-- first proof in agda

open import Agda.Builtin.Nat
open import Agda.Builtin.List

toto : Nat → Nat
toto x = zero

prede : Nat → Nat
prede zero = zero
prede (suc n) = n

concat : List Nat → List Nat  → List Nat
concat [] l2  = l2
concat (x ∷ l1) l2 = x ∷ concat l1 l2

popo = concat (1 ∷ 2 ∷ [])  (3 ∷ [])

longueur : List Nat → Nat
longueur [] = zero
longueur (x ∷ l) = 1 + (longueur l)


K : {A B : Set} → A → B → A
K x y = x

S : { A B C : Set} → (A → B → C) → (A → B) → (A → C)
S x y z = x z (y z)

I : {A : Set } → A → A
I x = x

coco = S K I










 