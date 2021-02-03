
(* The type of tokens. *)

type token = 
  | SYMBOLE of (string)
  | QUOTE
  | PARRIGHT
  | PARLEFT
  | NIL
  | MOT of (string)
  | ENTIER of (int)
  | BOOLEEN of (bool)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Scheme_exp.exp)
