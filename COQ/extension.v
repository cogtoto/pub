
Inductive even : nat -> Prop :=
  | EvenO : even 0
  | EvenS : forall n, even n -> even (S (S n))
.

Inductive valeur := Vrai | Faux.

Fixpoint checkEven (n : nat) : valeur  :=
    match n with
    | O => Vrai
    | 1 => Faux
    | S (S n') =>  checkEven n' 
  end.
  
Theorem pair1000 : even 1000.
repeat (apply EvenS).
apply EvenO.
Qed.
  
Theorem pair1000bis : checkEven 1000 = Vrai.
simpl. reflexivity.
Qed.

Theorem checkEvenisgood : forall (n:nat) , even n -> (checkEven n = Vrai).
intro n.
induction n. simpl. intro. reflexivity.
simpl.
