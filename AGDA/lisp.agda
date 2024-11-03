{-# OPTIONS --without-K #-}
-- lisp agda
-- Définition de l'interpréteur Lisp en Agda
open import Agda.Builtin.Equality
open import Agda.Builtin.Nat
open import Agda.Builtin.String
open import Agda.Builtin.Unit
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Maybe
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

-- leibniz equality : (P x → P y) → leib x y 
leib : {A : Set} ( x y : A ) → Set₁
leib {A} x y = (P : A → Set) → (P x) → (P y)

-- leib est réflexive
leib-refl : {A : Set} {x : A} → leib x x
leib-refl P Px = Px

-- leib est symétrique
leib-sym : {A : Set} {x y : A} → leib x y → leib y x
leib-sym {A} {x} {y} Lxy P = Qy
  where
    Q : A → Set
    Q z = P z → P x
    Qx : Q x
    Qx = leib-refl P
    Qy : Q y
    Qy = Lxy Q Qx 

subst : ∀ {A : Set} {x y : A} (P : A → Set) → x ≡ y → P x → P y
subst P refl px = px

≡→leib : {A : Set} {x y : A} → (x ≡ y) → (leib x y)
≡→leib xy P  =  subst P xy

leib→≡ : {A : Set} {x y : A} → (leib x y) → (x ≡ y)
leib→≡ {A} {x} {y} lxy = Qy
  where
    Q : A → Set
    Q z = x ≡ z
    Qx : Q x
    Qx = refl
    Qy : Q y
    Qy = lxy Q Qx

-- égalité de termes . la β-équivalence.
t1 = 2
t2 = 1 + 1

t1≡t2 : t1 ≡ t2
t1≡t2 = refl

-- plus0
cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f {x} {.x} refl = refl

plus0 : (n : Nat) → n + 0 ≡ n
plus0 zero = refl
plus0 (suc n) = cong suc (plus0 n)  

p0 : {n : Nat} → 0 + n ≡ n
p0  = refl

p0' : {n : Nat} → 0 + n ≡ n
p0' {zero} = refl
p0' {suc n} = cong suc (p0' {n})

K : Set₁
K =  {A : Set} {x : A} → (P : (x ≡ x) → Set) → P refl → (p : x ≡ x) → P p 

-- K-proof : K
-- K-proof refl = refl
-- unicity of identity proof UIP

UIP : Set₁
UIP =  {A : Set} {x y : A} (p q : x ≡ y) → p ≡ q

-- UIP-proof : UIP
-- UIP-proof refl refl = refl

URP : Set₁
URP =  {A : Set} {x : A} (p : x ≡ x ) → p ≡ refl

-- URP-proof : URP
-- URP-proof refl = refl

K→URP : K → URP
K→URP K p = K (λ p → p ≡ refl ) refl p 
 
J : ∀ {A : Set} {x : A} (P : (y : A) → x ≡ y → Set) → P x refl → (y : A) (p : x ≡ y) → P y p
J P Px y refl = Px


-- russell paradox
data ⊥ : Set where

exfalso : (A : Set) → ⊥ → A
exfalso a ()

-- unicity of reflexivity proof
-- URP : {A : Set} {x : A} (p : x ≡ x) → p ≡ refl
-- URP refl = refl

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

proj₁ : {A : Set} {B : A → Set} → Σ A B → A
proj₁ (a , _) = a

proj₂ : {A : Set} {B : A → Set} → (s : Σ A B) → B (proj₁ s)
proj₂ (a , b) = b

AC : {A B : Set}  {R : A → B → Set} → ((x : A) → Σ B (λ y → R x y)) → Σ (A → B)  (λ f → (x : A) → R x (f x))
AC {A} {B} {R} f = (λ x → proj₁ (f x)) , (λ x → proj₂ (f x))

coe : {A B : Set} → (p : A ≡ B) → A → B
coe refl x = x

subst' : {A B : Set} → (f : A → B) → (x y : A) → (x ≡ y) → (f x ≡ f y)
subst' f x .x refl = refl 


-- Définition de l'équivalence
record _≃_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
    to-from : (x : A) → from (to x) ≡ x
    from-to : (y : B) → to (from y) ≡ y

-- Axiome d'univalence
postulate
  univalence :  {A : Set} {B : Set} → ((A ≃ B)  ≃ (A ≡ B))


-- Définition d'un type somme de deux unités
data Two : Set where
  left  : Two
  right : Two

-- Définition de l'équivalence entre Bool et Two
boolToTwo : Bool → Two
boolToTwo true  = left
boolToTwo false = right

twoToBool : Two → Bool
twoToBool left  = true
twoToBool right = false

-- Preuve que boolToTwo et twoToBool sont des inverses l'un de l'autre
to-from : (b : Bool) → twoToBool (boolToTwo b) ≡ b
to-from true  = refl
to-from false = refl

from-to : (t : Two) → boolToTwo (twoToBool t) ≡ t
from-to left  = refl
from-to right = refl

-- Construction du record d'équivalence
boolEquivTwo : Bool ≃ Two
boolEquivTwo = record
  { to = boolToTwo
  ; from = twoToBool
  ; to-from = to-from
  ; from-to = from-to
  }