{
open Parser
open Syntax
type brace = BNone | BStr
let braces = ref []
}
rule token = parse
| [' ' '\t' '\n' '\r']+ { token lexbuf }
| ['0'-'9']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '+' { ADD }
| "*" { MUL }
| eof { EOF }
| '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' {STR s}
| '"' (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" {braces := BStr :: !braces; LSTR s}
| '{' { braces := BNone::!braces; LBRACE }
| '}' { match !braces with
        | BStr::xs -> braces := xs; str lexbuf
        | BNone::xs -> braces := xs; RBRACE
        | [] -> failwith "brace nest error" }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and str = parse
| (([^ '"' '\\'] | '\\' [^ '{'])* as s) "\\{" { braces := BStr :: !braces; ISTR s }
| (([^ '"' '\\'] | '\\' [^ '{'])* as s) '"' { RSTR s }
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
