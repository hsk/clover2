{
open Parser
open Syntax
let reg = ref true
let reg_add = ref ""
let reset () =
  reg := true;
  reg_add := ""
}
rule token = parse
| [' ' '\t' '\n' '\r']+ { token lexbuf }
| ['0'-'9']+ { reg := false; INT(int_of_string (Lexing.lexeme lexbuf)) }
| '+' { reg := true; ADD }
| "*" { reg := true; MUL }
| "/*" ([^ '*'] | '*' [^ '/'])* "*/" { token lexbuf }
| "//" [^ '\r' '\n']* { token lexbuf }
| "/" { if !reg then (reg_add := ""; regexp lexbuf) else (reg := true; DIV) }
| "/=" { if !reg then (reg_add := ""; regexp lexbuf) else (reg := true; DIVASSIGN) }
| eof { EOF }
| '(' { reg := true; LPAREN }
| ')' { reg := false; RPAREN }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and regexp = parse
| (([^ '/' '\\'] | '\\' _)* as s) '/' { reg := false; let a = !reg_add in reg_add:=""; REG(a^s) }
| eof { EOF }
| _ { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
