-- lambda calculus in agda

open import Agda.Builtin.Nat
open import Agda.Builtin.String 

data Terme : Set where
 v_  : String → Terme
 Λ   :  String → Terme → Terme
 _∘_ : Terme → Terme → Terme

omega = Λ "x" ((v "x") ∘ (v "x"))
Omega = omega ∘ omega

infix 9 _[_:=_]

_[_:=_] : Terme → String → Terme → Terme
(v x) [ y := V ] with (primStringEquality x y)
... | yes          =  V
... | no           = v x

