/* file: schemeyacc.mly */
%{
open List	
open Scheme_exp
%}

%token <string> SYMBOLE MOT 
%token <bool> BOOLEEN
%token <int> ENTIER
%token PARLEFT PARRIGHT NIL  QUOTE

%start main
%type <Scheme_exp.exp>  main

%% /* Grammar rule*/

main:    exp { $1 }
;

exp:      SYMBOLE	 	    { Symbole($1) }
		    | BOOLEEN 			{ Booleen($1) }
				| NIL 					{ Nil }
				| ENTIER 				{ Entier($1) }
				| MOT           { Mot($1) }
				| QUOTE exp     { Paire(Symbole("quote"), $2)}
        | PARLEFT paire  PARRIGHT { $2 }	
;

paire:			  |             { Nil }
							| SYMBOLE   	{Paire (Symbole($1), Nil)}
							| BOOLEEN    	{Paire (Booleen($1), Nil)}
							| ENTIER 	     {Paire (Entier($1), Nil)}
							| MOT 	       {Paire (Mot($1), Nil)} 
							| exp paire    {Paire ($1, $2)}
;      	
%%
