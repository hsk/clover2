{
open Parser
type brace = BNone | BStr | BReg
let braces = ref []
let reg = ref true
let reset () =
  braces := [];
  reg := true
let r t = reg := true; t
let n t = reg := false; t
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
| float { n(FLOAT(1.0)) }
| integer { n(INTEGER(1)) }
| hex_number { n(INTEGER(1)) }
| oct_number { n(INTEGER(1)) }
| "true" { n(BOOLEAN(true)) }
| "false" { n(BOOLEAN(false)) }
| "null" { n NULL }
| "++" { r INC }
| "--" { r DEC }
| '~' { r TIL }
| '!' { r NOT }
| "implements" { r IMPLEMENTS }
| '*' { r MUL }
| '%' { r MOD }
| '+' { r ADD }
| "-" { r SUB }
| ">>" { r RSH }
| "<<" { r LSH }
| ">=" { r GE }
| "<=" { r LE }
| ">" { r GT }
| "<" { r LT }
| "==" { r EQ }
| "!=" { r NE }
| '&' { r AND }
| '^' { r XOR }
| '|' { r OR }
| "&&" { r LAND }
| "||" { r LOR }
| '?' { r QUEST }
| ':' { r COLON }
| eof { EOF }
| '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' { n(STR s) }
| '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" { braces := BStr :: !braces; r(LSTR s) }
| ['p' 'P'] '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' { n(PSTR s) }
| ['p' 'P'] '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BStr :: !braces; r(PLSTR s)}
| ['b' 'B'] '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' { n(BSTR s) }
| ['b' 'B'] '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BStr :: !braces; r(BLSTR s)}
| '/' { if !reg then regexp lexbuf else r DIV  }
| "'" ('\\' _ as c) "'" { n(CHR c) }
| "'" ([^ '\\'] as c) "'" { n(CHR (String.make 1 c)) }
| '(' { r LPAREN }
| ')' { n RPAREN }
| '{' { braces := BNone::!braces; r LBRACE }
| '}' { match !braces with
        | BStr::xs -> braces := xs; str lexbuf
        | BReg::xs -> braces := xs; regexp_str lexbuf
        | BNone::xs -> braces := xs; n RBRACE
        | [] -> failwith "paren nest error" }
| word as w { n(WORD w) }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and str = parse
| (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BStr :: !braces; n(ISTR s)}
| (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' { n(RSTR s) }
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and regexp = parse
| (([^ '/' '\\'] | '\\' [^ '{'])* as s) '/' (['g' 'i' 's' 'm' 'A' 'D' 'U' 'x']* as o) { n(REG_STR(s,o)) }
| (([^ '/' '\\'] | '\\' [^ '{'])* as s) "\\{" { braces := BReg :: !braces; r(REG_LSTR s) }
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and regexp_str = parse
| (([^ '/' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BReg :: !braces; r(REG_ISTR s)}
| (([^ '/' '\\'] | '\\' [^ '{'])* as s) '/' (['g' 'i' 's' 'm' 'A' 'D' 'U' 'x']* as o) { n(REG_RSTR(s,o)) }
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
