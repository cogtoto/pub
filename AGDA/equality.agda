-- equality

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x 
  
data entier : Set where
  zero : entier
  succ : entier → entier

data bool : Set where
  true : bool
  false : bool

th0 : zero ≡ zero
th0 = refl {entier} {zero}

-- Le faux
data ⊥ : Set where

-- négation
neg : Set → Set 
neg A = A → ⊥

elim-faux : (P : ⊥ → Set) → (x : ⊥) → P x
elim-faux P () 

elim-faux2 : (P : Set) →  ⊥ → P
elim-faux2 P () 

th2 :  (zero ≡ succ zero) → ⊥
th2 ()

data Dec (A : Set) : Set where
  yes : A → Dec A
  no  : neg A → Dec A

RelDec : {A : Set} ( R : A → A → Set) → Set
RelDec {A} R = (x y : A) → (Dec (R x y))

lemme :  neg (false ≡ true) 
lemme = λ () 

th4 : RelDec {bool} _≡_
th4 true true = yes refl
th4 false false = yes refl 
th4 false true =  no lemme 
th4 true false = no (λ ())

suc-injective : {n m : entier} → (succ n ≡ succ m) → (n ≡ m)
suc-injective {n} {.n} refl = refl

th5 : RelDec {entier} _≡_
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

th_f : (fac trois) ≡ (trois * trois) → ⊥
th_f = λ ()

{-# BUILTIN NATURAL entier #-} 

data Liste (A : Set) : Set where
  vide : Liste A
  cons : A → Liste A → Liste A

l1 = cons 1 (cons 2 (cons 3 vide))
l2 = cons 4 (cons 5 (cons 6 vide))

append : {A : Set} → Liste A → Liste A → Liste A
append vide ys = ys
append (cons x xs) ys = cons x (append xs ys)

l3 = append l1 l2

rev : {A : Set} → Liste A → Liste A
rev vide = vide
rev (cons x xs) = append (rev xs) (cons x vide)

l4 = rev l3

length : {A : Set} → Liste A → entier
length vide = zero
length (cons x xs) = succ (length xs)

cong : {A B : Set} → {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl  

th-l : (l1 l2 : Liste entier) → (length (append l1 l2)) ≡ (length l1 + length l2)
th-l vide l2 = refl
th-l (cons x l5) l2 = cong succ (th-l l5 l2)

th-o : {n : entier} → (n + zero) ≡ n
th-o {zero} = refl
th-o {succ n} = cong succ (th-o {n})

