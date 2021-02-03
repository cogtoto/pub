(* file: lambdalexical.mll   *)
{
open Lex
open Printf
open Terme
}

let texte = ['a'-'z'] ['a'-'z' '0'-'9']*
rule lexana = parse
	| "lambda"	{ LAMBDA }
	| '.' { POINT }
  | texte as varia	{ VARIABLE (varia) }
  | '('		{ PARLEFT }
  | ')'		{ PARRIGHT }
  | '\n' {NEWLINE}
	| _		{ lexana lexbuf }
	| eof		{ raise End_of_file }

{
	exception Fin
  exception Erreur of string
	
  let _ =
    let lexbuf = Lexing.from_channel stdin in
				
		let rec exprule courant =
		match courant with
		| VARIABLE(x) -> Var(x)
		| PARLEFT -> parrule (lexana lexbuf)
		| NEWLINE -> raise Fin
		| _ ->  raise (Erreur "exprule")
		
		and parrule courant =
			  match courant with
				| LAMBDA -> lambdarule courant
				| _ -> apprule courant
		 
		and apprule courant =
				 let op1 = exprule courant in
			   let op2 = exprule (lexana lexbuf) in
				 let suivant = lexana lexbuf in (* consume PARRIGHT*)
				 match suivant with 
				| PARRIGHT ->  App(op1, op2) 
				| _ -> raise (Erreur "apprule")
			   
		  		
		and lambdarule courant =
			  let var = lexana lexbuf in 
				let _ = lexana lexbuf in  (* consume POINT *)
				let corps = exprule(lexana lexbuf) in
				let _ =  lexana lexbuf (* consume PARRIGHT *) in
				match var with 
				| VARIABLE(x) -> Lam(x, corps)
				| _ -> raise (Erreur "lambdarule")
			
		in (betaNormalPrint (exprule (lexana lexbuf)); flush stdout)

}