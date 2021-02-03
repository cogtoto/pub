
(* The type of tokens. *)

type token = 
  | SYMBOLE of (string)
  | QUOTE
  | PARRIGHT
  | PARLEFT
  | OR
  | NIL
  | MOT of (string)
  | LET
  | LAMBDA
  | IF
  | ENTIER of (int)
  | DEFINE
  | COND
  | BOOLEEN of (bool)
  | BEGIN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Scheme_exp.exp)
