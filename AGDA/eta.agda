-- typed lambda calculus 

open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.String

data type : Set where
  type_var : String → type
  type_arrow : type → type → type

data terme : Set where
  bvar : Nat → terme
  fvar : String → terme
  abs : terme → terme
  app : terme → terme → terme


  