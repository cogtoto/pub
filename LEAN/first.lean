inductive Expr where
  | const : Nat → Expr
  | add   : Expr → Expr → Expr
  | mul   : Expr → Expr → Expr
deriving Repr

-- (2 + 3) * 4
def e : Expr := .mul (.add (.const 2) (.const 3)) (.const 4)

def eval : Expr → Nat
  | .const n   => n
  | .add a b   => eval a + eval b
  | .mul a b   => eval a * eval b

#eval eval e  -- 20
