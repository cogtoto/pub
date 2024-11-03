-- halt

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

g : Bool → Bool
g false = true 
g true = false

-- halt rend true si le pg x s'arrête 
halt : Nat →  Bool

-- soit le pg f, 
{-# TERMINATING #-}
f : Nat → Nat
f x with (halt (f x))
...       | true = f x
...       | false = 1
