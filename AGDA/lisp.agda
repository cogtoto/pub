-- lisp agda
-- Définition de l'interpréteur Lisp en Agda
open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Maybe
open import Agda.Builtin.Sigma
open import Agda.Builtin.Char

if_then_else_ : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

data _×_ (A : Set) (B : Set) : Set where
  _,_ : A → B → A × B

Env : Set

data Expr : Set where
  Lit    : Nat → Expr
  Var    : String → Expr
  Lambda : String → Expr → Expr
  App    : Expr → Expr → Expr
  Clos   : Expr × Env → Expr
  Plus   : Expr → Expr → Expr
  Quote  : Expr → Expr
  Y      : Expr → Expr
  Eval   : Expr → Expr

-- Environnement de liaison des variables
Env = List (String × Expr)

isvalue : Expr → Bool
isvalue (Lit _) = true
isvalue (App (Lit _) _) = true
isvalue (Clos _) = true
isvalue _ = false

-- Recherche dans l'environnement
lookup :  String → Env  →  Expr
lookup key [] = Lit 1999 
lookup key ((k , v) ∷ env) with primStringEquality key k
lookup key ((k , v) ∷ env) | true  =  v
lookup key ((k , v) ∷ env) | false = lookup key env

interleaved mutual
  eval : Env → Expr → Nat → Expr

  -- Reduction des expressions, call by value
  reduce : Env  → Expr → Nat → Expr
  reduce env e 0 = e
  reduce env (Lambda x c) _ = Clos ((Lambda x c) , env)
  reduce env (Lit l) _ = Lit l
  reduce env (Var x) _ = lookup x env
  reduce env (Clos (l , e')) _ = Clos (l , e')
  reduce env (Quote q) _ = q -- ne sert pas, voir fonction eval
  reduce env (Eval q) (suc n)  = (eval env q n)
  reduce env (Plus (Lit n1) (Lit n2)) (suc n) = Lit (n1 + n2)
  reduce env (Plus (Lit n1)  e2) (suc n) = Plus (Lit n1) (reduce env e2 n)
  reduce env (Plus  e1  e2) (suc n) = Plus (reduce env e1 n) (reduce env e1 n) 
  reduce env (Y e) (suc n)     =  App e (reduce env (Y e) n) -- reduction de l'opérateur Y
  reduce env (App e1 e2) (suc n) with (reduce env e1 n)
  reduce env (App e1 e2) (suc n)    | (Clos ((Lambda x corps) , env')) = reduce ((x , e2) ∷ env') corps n
  reduce env (App e1 e2) (suc n)    | t    = App t (reduce env e2 n)

  -- sémantique de l'appel de valeur
  eval env e 0 = e 
  eval env (Quote q) _ = q
  eval env e (suc n) = 
    let e1 = reduce env e n in
     if (isvalue e1) then e1 else (eval env e1 n)

-- Exemple d'utilisation de l'interpréteur
exampleExpr : Expr
exampleExpr = App (Lambda "y" (App (Var "y") (Lit 1976))) (Lambda "x" (Var "x"))

omega : Expr
omega = Lambda "x" (App (Var "x") (Var "x"))

Omega : Expr
Omega = App omega (Lambda "x" (App (Var "x") (Var "x")))

K : Expr
K = Lambda "a" (Lambda "b" (Var "a"))
 
plus1 : Expr
plus1 = Lambda "n" (Plus (Lit 1) (Var "n"))

output :  Expr
output = eval [] (App plus1 (Lit 14)) 10

-------------------------------------------------------------------
data Fin : Nat → Set where
  zero : { n : Nat } → Fin (suc n)
  suc  : { n : Nat} → Fin n → Fin (suc n)


data Pair : Nat → Set where
  pair0 : Pair 0
  pairSS : (n : Nat) → Pair n → Pair (suc (suc n))

pair6 : Pair 6
pair6 =  pairSS 4 (pairSS 2 (pairSS zero pair0))

data False : Set where

data True : Set where
  tt : True

pair1 : Pair 1 → False
pair1 ()

ispair? : Nat → Set
ispair? zero = True
ispair? (suc zero) = False
ispair? (suc (suc n)) = ispair? n 

ispairxsound : {n : Nat} → ispair? n → Pair n
ispairxsound {zero} tt = pair0
ispairxsound {1} () 
ispairxsound {suc (suc n)} x = pairSS n (ispairxsound {n} x)

th : Pair 90
th = ispairxsound tt





