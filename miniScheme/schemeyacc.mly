/* file: schemeyacc.mly */
%{
open List	
open Scheme_exp
%}

%token <string> SYMBOLE MOT 
%token <bool> BOOLEEN
%token <int> ENTIER


%token PARLEFT PARRIGHT IF AND OR DEFINE  QUOTE LAMBDA LET COND
%token NIL BEGIN 

%start main
%type <Scheme_exp.exp>  main

%% /* Grammar rule*/

main:    exp { $1 }
;

exp:      SYMBOLE	 			{ Var($1) }
		    | BOOLEEN 			{ Literal(Booleen($1)) }
				| NIL 					{ Literal(Nil) }
				| ENTIER 				{ Literal(Entier($1)) }
				| MOT           { Literal(Mot($1)) }
				| PARLEFT LET PARLEFT bindliste PARRIGHT exp PARRIGHT { Let($4,$6) }
				| PARLEFT COND exp_cond PARRIGHT								     { Cond($3)} 
				| PARLEFT QUOTE exp PARRIGHT { Literal (Quote $3) }
        | PARLEFT liste { $2 }	
;

bindliste:
          NIL { [] }
					| bindpair { $1 :: [] }
          | bindliste  bindpair  { List.append $1 ($2 :: [])}
;

bindpair: PARLEFT SYMBOLE exp PARRIGHT {  ($2,$3) }
;

exp_cond:
         NIL { [] }
				 | cond_pair { $1 :: []}
				 | exp_cond cond_pair  { List.append $1 ($2 :: [])}
;

cond_pair: PARLEFT exp exp PARRIGHT {  ($2,$3) }
;

liste:  IF exp exp exp PARRIGHT          									 { If($2, $3, $4)}
        | AND exp exp PARRIGHT           									 { And($2, $3)}
				| OR exp exp PARRIGHT            									 { Or($2, $3)}
				| LAMBDA NIL exp PARRIGHT                          { Lambda([], $3) }
				| LAMBDA PARLEFT symbliste PARRIGHT exp PARRIGHT   { Lambda($3, $5) }
		  	| DEFINE SYMBOLE exp PARRIGHT											 { Define($2,$3)}
			  | exp largs PARRIGHT          									 		 { Call($1, $2 ) }
				| exp      PARRIGHT          									 		 { Call0($1) }
				| BEGIN largs PARRIGHT															 { Begin($2)}
;
   
symbliste:
			| SYMBOLE 	{$1 :: []}
			| symbliste SYMBOLE {List.append $1 ($2 :: []) }

largs:
      exp 				{$1 :: []}
			| largs exp {List.append $1 ($2 :: []) }
;      	
%%
