Require Import Arith.
Require Import ZArith.
Require Import Bool.
Require Import List.

Inductive exp : Set :=
  | Const : nat -> exp
  | Fois : exp -> exp -> exp
  | Plus : exp -> exp -> exp.

Fixpoint expEval (e:exp) : nat :=
  match e with
  | Const n => n
  | Fois e1 e2 => mult  (expEval e1) (expEval e2)
  | Plus e1 e2 => plus (expEval e1) (expEval e2)
  end.

Inductive instr : Set :=
 | EMPILER : nat -> instr
 | ADD :  instr
 | MUL : instr
.

Definition programme := list instr.
Definition pile := list nat.

Definition instrExec (i:instr) (s: pile) : option pile:=
  match i with
  | EMPILER n => Some (n :: s)
  | ADD => 
     match s with
     | arg1 :: arg2 :: s' => Some (expEval (Plus (Const(arg1)) (Const(arg2))) :: s')
     | _ => None
     end
  | MUL =>
     match s with
     | arg1 :: arg2 :: s' => Some (expEval (Fois (Const(arg1)) (Const(arg2))) ::s')
     | _ => None
     end
  end.
     
Fixpoint progExec (p:programme)  (s:pile) : option pile :=
  match p with 
  | nil => Some s
  | i :: p' => 
      match instrExec i s with
      | None => None
      | Some s' => progExec p' s' 
      end
  end. 

Fixpoint compile (e:exp) : programme :=
  match e with 
  | Const n => EMPILER n :: nil
  | Plus e1 e2 => compile e2 ++ compile e1 ++ ADD :: nil
  | Fois e1 e2 => compile e2 ++ compile e1 ++ MUL :: nil
  end .
        
Eval compute in (compile (Const 1999)) .
Eval compute in (compile (Fois (Plus (Const 1999) (Const 1)) (Const 5))) .
Eval compute in ( progExec (compile (Fois (Plus (Const 1999) (Const 1)) (Const 5))) nil) .

Lemma compile_correct_lemme: forall (e:exp) (p: programme) (s: pile),
                            progExec (compile e++p) s = progExec p (expEval e::s)
 .
 induction e.
 intros.
 unfold compile.
 unfold expEval.
 unfold progExec at 1.
 simpl.
 fold progExec.
 reflexivity.

 intros.
 unfold compile. fold compile.
 unfold expEval. fold expEval.
 rewrite app_assoc_reverse.
 rewrite IHe2. rewrite app_assoc_reverse.
 rewrite IHe1.
 unfold progExec at 1. simpl. fold progExec. reflexivity.

 intros.
 unfold compile.  fold compile. 
 rewrite app_assoc_reverse. rewrite IHe2.
 rewrite app_assoc_reverse. 
 unfold progExec at 1. simpl. fold progExec.
 rewrite IHe1. 
 unfold progExec at 1. simpl. fold progExec. reflexivity.

 Qed.

Theorem compile_correct: forall e : exp, Some ((expEval e) :: nil) = (progExec (compile e) nil).
intros.
rewrite (app_nil_end (compile e)).
rewrite compile_correct_lemme.
reflexivity.
Qed.

Print compile_correct.