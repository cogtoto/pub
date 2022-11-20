Require Import Ascii String.
Open Scope string_scope.
Definition Q : string := "
Eval compute in ""
Require Import Ascii String.
Open Scope string_scope.
Definition Q : string := "" ++ """" ++ Q ++ """" ++ ""."" ++ Q.".
Eval compute in "
Require Import Ascii String.
Open Scope string_scope.
Definition Q : string := " ++ """" ++ Q ++ """" ++ "." ++ Q.
