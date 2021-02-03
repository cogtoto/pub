(* file: schemelex.mll *)
{
  open Schemeyacc (* Assumes the parser file is "schemeyacc.mly" *)
  exception Unexpected_token
  exception Eof
}

let booleen = "#t"|"#f"
let symbole = ['a'-'z'] ['a'-'z' '?' '!' '-' '0'-'9'] * | ['+' '-' '*' '=' '<' '>'] *  
let entier = ['0'-'9'] ['0'-'9']*
let mot = '"' ['a'-'z']  ['a'-'z' '?' '>' '0'-'9' ' ']* '"'
let comment = ';' ['a'-'z' '?' '0'-'9' ' ']* '\n'

rule token = parse
  |  [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  |  comment              { token lexbuf }     (* skip comments *)
  |  "nil" |  "()"	{ NIL }
  |  booleen as bo  { if (bo = "#t") then BOOLEEN(true) else BOOLEEN(false) }
  |  symbole	as sym { SYMBOLE (sym) }
  |  entier as ent { ENTIER (int_of_string ent) }
  |  mot as m { MOT (m)}
  |  '\''    { QUOTE }
  |  '('		{ PARLEFT }
  |  ')'		{ PARRIGHT }
  |  eof		{  raise Eof }
  |  _ { raise Unexpected_token }
