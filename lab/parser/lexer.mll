{
open Parser
type brace = BNone | BStr
let braces = ref []
}
let alpha       = ['a'-'z' 'A'-'Z']
let num         = ['0'-'9']
let num_postfix = ['y' 's' 'l' 'u'] | "uy" | "us" | "ul"
let float       = ['0'-'9']+ '.' ['0'-'9']+ ['f' 'F']?
let integer     = ['1'-'9'] ['0'-'9']* num_postfix?
let hex_number  = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ num_postfix?
let oct_number  = '0' ['0'-'7']* num_postfix?
(*
let slash_word  = ("./"|"../")? alpha (alpha|'_'|'/')*
*)
let word        = alpha (alpha | '_')*
(*
let annotation  = '@' (alpha (alpha | num | '-' | '_' | '[' | ']')* )?
let ann         = '@' (alpha | '_')*
*)
rule token = parse
| [' ' '\t' '\n' '\r']+ { token lexbuf }
| float { FLOAT(1.0) }
| integer { INTEGER(1) }
| hex_number { INTEGER(1) }
| oct_number { INTEGER(1) }
| "true" { BOOLEAN(true) }
| "false" { BOOLEAN(false) }
| "null" { NULL }
| "++" { INC }
| "--" { DEC }
| '~' { TIL }
| '!' { NOT }
| "implements" { IMPLEMENTS }
| '*' { MUL }
| "/" { DIV }
| '%' { MOD }
| '+' { ADD }
| "-" { SUB }
| ">>" { RSH }
| "<<" { LSH }
| ">=" { GE }
| "<=" { LE }
| ">" { GT }
| "<" { LT }
| "==" { EQ }
| "!=" { NE }
| '&' { AND }
| '^' { XOR }
| '|' { OR }
| "&&" { LAND }
| "||" { LOR }
| '?' { QUEST }
| ':' { COLON }
| eof { EOF }
| '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' {STR s}
| '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BStr :: !braces; LSTR s}
| "'" ('\\' _ as c) "'" { CHR c }
| "'" ([^ '\\'] as c) "'" { CHR (String.make 1 c) }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { braces := BNone::!braces; LBRACE }
| '}' { match !braces with
        | BStr::xs -> braces := xs; str lexbuf
        | BNone::xs -> braces := xs; RBRACE
        | [] -> failwith "paren nest error" }
| word as w { WORD w }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and str = parse
| (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BStr :: !braces; ISTR s}
| (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' {RSTR s}
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
