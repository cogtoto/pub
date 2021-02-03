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

exp:      SYMBOLE	 	    {  Symbole($1) }
		    | BOOLEEN 			{  Booleen($1) }
				| NIL 					{  Nil }
				| ENTIER 				{  Entier($1) }
				| MOT           {  Mot($1) }
				| QUOTE exp     {  Paire(ref (Symbole("quote")), ref($2))}
        | PARLEFT paire  PARRIGHT { $2 }	
;

paire:			  |              {Nil }
							| SYMBOLE   	 {Paire (ref (Symbole($1)), ref Nil)}
							| BOOLEEN    	 {Paire (ref (Booleen($1)), ref Nil)}
							| ENTIER 	     {Paire (ref (Entier($1)), ref Nil)}
							| MOT 	       {Paire (ref (Mot($1)), ref Nil)} 
							| exp paire    {Paire (ref($1), ref($2))}
;      	
%%
