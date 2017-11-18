%{
let div_mode = ref false
%}
%token <string> STR
%token <string> LSTR
%token <string> ISTR
%token <string> RSTR
%token <string> PSTR
%token <string> PLSTR
%token <string> BSTR
%token <string> BLSTR
%token <string*string> REG_STR
%token <string> REG_LSTR
%token <string> REG_ISTR
%token <string*string> REG_RSTR
%token <string> CHR
%token <int> INTEGER
%token <float> FLOAT
%token <bool> BOOLEAN
%token <string> ANNOTATION
%token NULL
%token <string> WORD
%token <string> SLASH_WORD
%token LIST ELIST SLIST ARRAY EARRAY SARRAY HASH TUPLE
%token LBRACK RBRACK
%token RETURN NEW BREAK FOR WHILE IF ELIF ELSE TRY CATCH THROW LAMBDA CLOSURE DEF INHERIT
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
%token COMMA SEMI DOT
%token COLON_ASSIGN ASSIGN
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN LSH_ASSIGN RSH_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN
%token ADDADD SUBSUB ARROW DOTDOT
%token EOF
%left INC DEC TIL ADD SUB
%left RETURN
%type <int> expression
%start expression
%type <int> type_
%start type_
%%

/*
class_type        : WORD { 1 }
*/
type_             : LAMBDA type_option type_array_opt annotation_opt { 1 }
                  | LAMBDA LPAREN type_list RPAREN type_option type_array_opt annotation_opt { 1 }
                  | WORD type_params_opt type_array_opt annotation_opt { 1 }
annotation_opt    : /* empty */ { 1 }
                  | ANNOTATION { 1 }
type_array_opt    : /* empty */ { 1 }
                  | LBRACK RBRACK { 1 }
type_params_opt   : /* empty */ { 1 }
                  | LT type_list1 GT { 1 }
type_list         : /* empty */ { 1 }
                  | type_list1 { 1 }
type_list1        : type_ { 1 }
                  | type_ COMMA type_list1 { 1 }

literal           : INTEGER { 1 }
                  | ADD INTEGER { 1 }
                  | SUB INTEGER { 1 }
                  | FLOAT { 1 }
                  | ADD FLOAT { 1 }
                  | SUB FLOAT { 1 }
                  | BOOLEAN { 1 }
                  | NULL { 1 }
                  | charactor_literal { 1 }
                  | string_literal { 1 }
                  | path_literal   { 1 }
                  | buffer_literal { 1 }
                  | regexp_literal { 1 }

charactor_literal : CHR                                { 1 }
string_literal    : STR                                { 1 }
                  | LSTR string_expression RSTR        { 1 }
string_expression : expression ISTR string_expression  { 1 }
                  | expression                         { 1 }
path_literal      : PSTR                               { 1 }
                  | PLSTR string_expression RSTR       { 1 }
buffer_literal    : BSTR                               { 1 }
                  | BLSTR string_expression RSTR       { 1 }
regexp_literal    : REG_STR                             { 1 }
                  | REG_LSTR regexp_expression REG_RSTR { 1 }
regexp_expression : expression REG_ISTR regexp_expression { 1 }
                  | expression                            { 1 }

collection_expression
                  : list_ {1}
                  | equalable_list { 1 }
                  | sortable_list { 1 }
                  | array_value { 1 }
                  | array { 1 }
                  | equalable_array { 1 }
                  | sortable_array { 1 }
                  | hash { 1 }
                  | tuple { 1 }

array_value       : LBRACK expressions RBRACK { 1 }
array             : ARRAY LBRACE expressions RBRACE { 1 }
equalable_array   : EARRAY LBRACE expressions RBRACE { 1 }
sortable_array    : SARRAY LBRACE expressions RBRACE { 1 }
list_             : LIST LBRACE expressions RBRACE { 1 }
equalable_list    : ELIST LBRACE expressions RBRACE { 1 }
sortable_list     : SLIST LBRACE expressions RBRACE { 1 }
hash              : HASH LBRACE expression_pairs RBRACE { 1 }
tuple             : TUPLE LBRACE expressions RBRACE { 1 }

expressions       : /* empty */ { 1 }
                  | expressions1 { 1 }
expressions1      : expression { 1 }
                  | expression COMMA expressions1 { 1 }

expression_pairs  : /* empty */ { 1 }
                  | expression_pairs1 { 1 }
expression_pairs1 : expression_pair { 1 }
                  | expression_pair COMMA expression_pairs1 { 1 }
expression_pair   : expression COLON expression { 1 }

control_expression: normal_block { 1 }
                  | if_expression { 1 }
                  | while_expression { 1 }
                  | for_expression { 1 }
                  | BREAK { 1 }
                  | throw_expression { 1 }
                  | try_expression { 1 }
                  | return_expression { 1 }
                  | new_expression { 1 }
                  | CLOSURE block_object { 1 }
                  | LAMBDA block_object { 1 }
                  | function_ { 1 }
                  | inherit_ { 1 }

normal_block      : block { 1 }
if_expression     : IF LPAREN expression RPAREN block elif { 1 }
elif              : else_block { 1 }
                  | elif1 { 1 }
elif1             : ELIF LPAREN expression RPAREN block else_block { 1 }
                  | ELIF LPAREN expression RPAREN block elif1 { 1 }
else_block        : /* empty */ { 1 }
                  | ELSE block { 1 }
while_expression  : WHILE LPAREN expression RPAREN block { 1 }
for_expression    : FOR LPAREN expression SEMI expression SEMI expression RPAREN block { 1 }
throw_expression  : THROW expression { 1 }
try_expression    : TRY block CATCH LPAREN param_list RPAREN block { 1 }
return_expression : RETURN expression { 1 }
                  | RETURN { 1 }
new_expression    : NEW type_for_new method_params { 1 }

block_object      : LPAREN param_list RPAREN type_option block { 1 }
type_option       : /* empty */ { 1 }
                  | COLON type_ { 1 }

function_         : DEF WORD LPAREN param_list RPAREN type_option block { 1 }
inherit_          : INHERIT method_params { 1 }

param_list        : /* empty */ { 1 }
                  | param_list1 { 1 }
param_list1       : param { 1 }
                  | param COMMA param_list1 { 1 }
param             : WORD COLON type_ { 1 }
                  
block             : LBRACE statement RBRACE { 1 }
statement         : /* empty */ { 1 }
                  | statement1 { 1 }
statement1        : expression SEMI { 1 }
                  | expression { 1 }
                  | expression SEMI statement1 { 1 }
                  | expression statement1 { 1 }
                  
type_for_new      : WORD /*('<' type_list '>')? ('[' expression "]")?*/ { 1 }
                  
slash_word        : SLASH_WORD { 1 }
                  | WORD { 1 }

assign_expression : slash_word COLON_ASSIGN expression { 1 }
                  | slash_word COLON type_ ASSIGN expression { 1 }
                  | slash_word ASSIGN expression { 1 }
                  | slash_word assign_operator expression { 1 }
assign_operator   : ADD_ASSIGN { 1 }
                  | SUB_ASSIGN { 1 }
                  | MUL_ASSIGN { 1 }
                  | DIV_ASSIGN { 1 }
                  | LSH_ASSIGN { 1 }
                  | RSH_ASSIGN { 1 }
                  | AND_ASSIGN { 1 }
                  | XOR_ASSIGN { 1 }
                  | OR_ASSIGN { 1 }
assign_operator_or_eq:
                  | EQ { 1 }
                  | assign_operator { 1 }

method_expression : slash_word type_ DOT WORD method_params { 1 }
                  | slash_word type_ DOT WORD assign_operator_or_eq expression { 1 }
                  | slash_word method_params { 1 }
                  /*
                  | slash_word command_method_params
                    ("||" word command_method_params|"&&"|SEMI|"\n")*
*/
method_params     : LPAREN expression_anns RPAREN simple_lambda_params_opt { 1 }
                  | simple_lambda_params_opt { 1 }
expression_anns   : /* empty */ { 1 }
                  | expression_anns1 { 1 }
expression_anns1  : expression ann { 1 }
                  | expression ann COMMA expression_anns1 { 1 }
ann               : /* empty */ { 1 }
                  | ANNOTATION { 1 }

simple_lambda_params_opt:
                  | /* empty */ { 1 }
                  | simple_lambda_params { 1 }
simple_lambda_params:
                  | LBRACE OR param_list OR type_option statement RBRACE { 1 }
                  | LBRACE statement RBRACE { 1 }
/*
command_method_params
                  : SEMI | "\n" | '${' (not RBRACE)* RBRACE
                    | "$" (alpha|num|'_')* | '\' . | '"' . * '"' | "'" . * "'" *
*/
expression_node   : collection_expression { 1 }
                  | control_expression { 1 }
                  | assign_expression { 1 }
                  | method_expression { 1 }
                  | literal { 1 }
                  | slash_word { 1 }
                  | LPAREN expression RPAREN { 1 }
                  | AND expression { 1 }

postposition_operator:
                  | DOTDOT WORD { 1 }
                  | DOTDOT WORD method_params { 1 }
                  | DOTDOT WORD assign_operator_or_eq expression { 1 }
                  | LBRACK expression RBRACK { 1 }
                  | LBRACK expression RBRACK assign_operator_or_eq expression { 1 }
                  | ARROW type_ { 1 }
                  | ARROW type_ EQ expression { 1 }
                  | ADDADD { 1 }
                  | SUBSUB { 1 }
postposition_operators:
                  | /* empty */ { 1 }
                  | postposition_operator postposition_operators { 1 }
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
expression_conditional:
                  | expression_lor                                   { 1 }
                  | expression_lor QUEST expression COLON expression { 1 }
                  
expression        : expression_conditional { 1 }
