%{
open Syntax
%}
%token <int> INT
%token <string> REG
%token ADD MUL DIV DIVASSIGN
%token LPAREN RPAREN
%token EOF
%type <Syntax.e> exp
%start exp
%%

exp    : term                { $1 }
       | exp ADD term        { Add($1, $3) }
term   : fact                { $1 }
       | term MUL fact       { Mul($1, $3) }
       | term DIV fact       { Div($1, $3) }
       | term DIVASSIGN fact { DivAssign($1, $3) }
fact   : INT                 { Int $1 }
       | LPAREN exp RPAREN   { $2 }
       | REG                 { Reg $1 }
