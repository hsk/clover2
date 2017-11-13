%{
open Syntax
%}
%token <string> STR
%token <string> LSTR
%token <string> ISTR
%token <string> RSTR
%token <int> INT
%token ADD MUL
%token LBRACE RBRACE
%token EOF
%type <Syntax.e> exp
%start exp
%%

exp    : term              { $1 }
       | exp ADD term      { Add($1, $3) }
term   : fact              { $1 }
       | term MUL fact     { Mul($1, $3) }
fact   : INT               { Int $1 }
       | LBRACE exp RBRACE { $2 }
       | str    { $1 }

str    : STR               { Str $1 }
       | LSTR str_exp RSTR { List.fold_left(fun a b -> Add(a,b))
                             (Str $1) ($2@[Str $3]) }
str_exp: exp ISTR str_exp  { $1::(Str $2)::$3 }
       | exp               { [$1] }
