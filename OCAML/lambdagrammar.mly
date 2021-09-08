/* file: lambdagrammar.mly */
%{
open Terme
%}

%token <string> VARIABLE
%token LAMBDA PARLEFT PARRIGHT POINT 

%start exp
%type <Terme.terme> exp

%% /* Grammar rules and actions follow */
exp:      VARIABLE	{ Var($1) }
        | PARLEFT exp exp PARRIGHT		{ App($2, $3)}
        | PARLEFT LAMBDA VARIABLE POINT exp	PARRIGHT	{ Lam($3, $5) }
;
%%

