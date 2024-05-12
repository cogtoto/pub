-- equality

data egal {A : Set} : A → A → Set where
  refl : {x : A} → egal x x 
  
data entier : Set where
  zero : entier
  succ : entier → entier

data bool : Set where
  true : bool
  false : bool


th0 : egal zero zero
th0 = refl {entier} {zero}

-- Le faux
data Faux : Set where

-- négation
neg : Set → Set 
neg A = A → Faux

elim-faux : (P : Faux → Set) → (x : Faux) → P x
elim-faux P () 

elim-faux2 : (P : Set) →  Faux → P
elim-faux2 P () 

th2 :  (egal zero (succ zero)) → Faux
th2 ()

data Dec (A : Set) : Set where
  yes : A → Dec A
  no  : neg A → Dec A

RelDec : {A : Set} ( R : A → A → Set) → Set
RelDec {A} R = (x y : A) → (Dec (R x y))

lemme :  neg (egal false true) 
lemme = λ () 

th4 : RelDec {bool} egal
th4 true true = yes refl
th4 false false = yes refl 
th4 false true =  no lemme 
th4 true false = no (λ ())

suc-injective : {n m : entier} → (egal (succ n) (succ m)) → (egal n m)
suc-injective {n} {.n} refl = refl

th5 : RelDec {entier} egal
th5 zero zero = yes refl
th5 zero (succ y) = no (λ ())
th5 (succ x) zero = no (λ ())
th5 (succ x) (succ y) with (th5  x y)
th5 (succ x) (succ .x) | yes refl =  yes refl
th5 (succ x) (succ y) | no f =  no (λ x  →  (f (suc-injective x)))

_+_ : entier → entier → entier
zero + m = m
succ n + m = succ (n + m)

_*_ : entier → entier → entier
zero * m = zero
succ n * m = m + (n * m)

un = succ zero

pred : entier → entier 
pred zero = zero
pred (succ n) = n


fac : entier → entier
fac zero = un
fac (succ n) = succ n * (fac n)

deux = succ (succ zero)
quatre = deux + deux
seize = quatre * quatre
trois = deux + un

th_f : egal (fac trois) (trois * trois) → Faux
th_f = λ ()


  
  
  