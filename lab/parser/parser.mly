%{
%}
%token <string> STR
%token <string> LSTR
%token <string> ISTR
%token <string> RSTR
%token <string> CHR
%token <int> INTEGER
%token <float> FLOAT
%token <bool> BOOLEAN
%token NULL
%token <string> WORD
%token INC DEC TIL NOT
%token IMPLEMENTS
%token MUL DIV MOD
%token ADD SUB
%token LSH RSH
%token GE LE GT LT
%token EQ NE
%token AND
%token XOR
%token OR
%token LOR LAND
%token COLON QUEST
%token LPAREN RPAREN
%token LBRACE RBRACE
%token EOF
%type <int> expression
%start expression
%%

/*

class_type        : word
type              : "lambda" ('(' type_list? ')')? (':' type)? '[]'? annotation?
                    | word ('<' type_list '>')? '[]'? annotation?
type_list         : type (',' type)*
*/
literal           :
                  | INTEGER { 1 }
                  | ADD INTEGER { 1 }
                  | SUB INTEGER { 1 }
                  | FLOAT { 1 }
                  | ADD FLOAT { 1 }
                  | SUB FLOAT { 1 }
                  | BOOLEAN { 1 }
                  | NULL { 1 }
                  | charactor_literal { 1 }
                  | string_literal { 1 }
                  /*
                  | path_literal | buffer_literal | regexp_literal
                  */

charactor_literal : CHR                                { 1 }
string_literal    : STR                                { 1 }
                  | LSTR string_expression RSTR        { 1 }
string_expression : expression ISTR string_expression  { 1 }
                  | expression                         { 1 }

/*
path_literal      : ("P"|"p") '"' (not('"'|'\') | escape_sequence)* '"'
buffer_literal    : ("B"|"b") '"' (not('"'|'\') | escape_sequence)* '"'
regexp_literal    : '/' ('\' '\' '{' | '\' string_expression | not "/")* "/"
                      ('g'|'i'|'s'|'m'|'A'|'D'|'U'|'x')*

collection_expression
                  : list | equalable_list | sortable_list
                    | array_value | array | equalable_array | sortable_array
                    | hash | tuple

array_value       : '[' expressions ']'
array             : "array" '{' expressions '}'
equalable_array   : ("equalable_array" | "earray") '{' expressions '}'
sortable_array    : ("sortable_array" | "sarray") '{' expressions '}'
list              : "list" '{' expressions '}'
equalable_list    : ("equalable_list" | "elist") '{' expressions '}'
sortable_list     : ("sortable_list" | "slist") '{' expressions '}'
hash              : "hash" '{' (expression_pair (',' expression_pair)*)? '}'
tuple             : "tuple" '{' expressions '}'

expressions       : (expression (',' expression)*)?
expression_pair   : expression ":" expression

control_expression: normal_block | if_expression
                    | while_expression | for_expression | "break"
                    | throw_expression | try_expression
                    | return_expression | new_expression
                    | "closure" block_object | "lambda" block_object | function
                    | iniherit

normal_block      : block
if_expression     : "if" "(" expression ")" block
                      ("elif" "(" expression ")" block)*
                      ("else" block)?
while_expression  : "while" "(" expression ")" block
for_expression    : "for" "(" expression ";" expression ";" expression ")" block
throw_expression  : "throw" expression
try_expression    : "try" block "catch" "(" param_list ")" block
return_expression : "return" expression?
new_expression    : "new" type_for_new method_params
block_object      : "(" param_list ")" (':' type)? block 
function          : "def" word "(" param_list ")" (':' type)? block
iniherit          : "inherit" method_params

param             : word ":" type
param_list        : (param ("," param)*)?
block             : "{" (expression ";"?)* "}"
type_for_new      : word ('<' type_list '>')? ('[' expression "]")?

assign_expression : slash_word ':=' expression
                    | slash_word ':' type '=' expression
                    | slash_word '=' expression
                    | slash_word assign_operator expression
assign_operator   : "+="|"-="|"*="|"/="|"%="|"<<="|">>="|"&="|"^="|"|="

method_expression : slash_word type '.' word method_params
                    | slash_word type '.' word (assign_operator|'==') expression
                    | slash_word method_params
                    | slash_word command_method_params
                      ("||" word command_method_params|"&&"|";"|"\n")*

method_params     : ('(' (expression ann (',' expression ann)*)? ')')?
                      simple_lambda_params?
simple_lambda_params
                  : '{' ("|" param_list "|" (':' type)?)? (expression ";"?)* '}'
command_method_params
                  : ";" | "\n" | '${' (not '}')* '}'
                    | "$" (alpha|num|'_')* | '\' . | '"' . * '"' | "'" . * "'" *
*/
expression_node   : /*collection_expression*/
                  /*| assign_expression */
                  /*| method_expression */
                  | literal { 1 }
                  /*| slash_word */
                  | LPAREN expression RPAREN { 1 }
                  /*| '&' expression*/
/*
postposition_operator
                  : '..' word (method_params|(assign_operator|'==') expression)?
                    | '[' expression "]" ((assign_operator|'==') expression)?
                    | '->' type ('==' expression)?
                    | '++' | '--'
*/
postposition_operators
                  : { 1 }
expression_monadic: expression_node postposition_operators { 1 }
                  | INC expression_monadic { 1 }
                  | DEC expression_monadic { 1 }
                  | TIL expression_monadic { 1 }
                  | NOT expression_monadic { 1 }
expression_implements:
                  | expression_monadic implements             { 1 }
implements        : IMPLEMENTS WORD implements                { 1 }
                  |                                           { 1 }
expression_mult   : expression_implements { 1 }
                  | expression_mult MUL expression_implements { 1 }
                  | expression_mult DIV expression_implements { 1 }
                  | expression_mult MOD expression_implements { 1 }
expression_add    : expression_mult                       { 1 }
                  | expression_add ADD expression_mult    { 1 }
                  | expression_add SUB expression_mult    { 1 }
expression_shift  : expression_add                        { 1 }
                  | expression_shift LSH expression_add   { 1 }
                  | expression_shift RSH expression_add   { 1 }
expression_comparison:
                  | expression_shift                          { 1 }
                  | expression_comparison GE expression_shift { 1 }
                  | expression_comparison LE expression_shift { 1 }
                  | expression_comparison GT expression_shift { 1 }
                  | expression_comparison LT expression_shift { 1 }
expression_equal  : expression_comparison { 1 }
                  | expression_equal EQ expression_comparison
                                                          { 1 }
                  | expression_equal NE expression_comparison
                                                          { 1 }
expression_and    : expression_equal                      { 1 }
                  | expression_and AND expression_equal   { 1 }
expression_xor    : expression_and                        { 1 }
                  | expression_xor XOR expression_and     { 1 }
expression_or     : expression_xor                        { 1 }
                  | expression_or OR expression_xor       { 1 }
expression_lor    : expression_or                         { 1 }
                  | expression_lor LAND expression_or     { 1 }
                  | expression_lor LOR expression_or      { 1 }
expression_conditional
                  : expression_lor                                   { 1 }
                  | expression_lor QUEST expression COLON expression { 1 }
                  
expression        : expression_conditional { 1 }
