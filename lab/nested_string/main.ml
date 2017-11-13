let parse_string s =
	Lexer.braces := [];
	(Parser.exp Lexer.token (Lexing.from_string s))

let _ =
	Printf.printf "%s\n" (Syntax.show (parse_string "1"));
	Printf.printf "%s\n" (Syntax.show (parse_string "1*2+3*4"));
	Printf.printf "%s\n" (Syntax.show (parse_string "\"abc\"+1+2*3"));
	Printf.printf "%s\n" (Syntax.show (parse_string "\"a\\{1+2}c\""));
	Printf.printf "%s\n" (Syntax.show (parse_string "\"a\\{\"1\"+\"2\"}c\""));
	Printf.printf "%s\n" (Syntax.show (parse_string "\"a\\{1+ \"22\\{2+\"aaa\"+2}22\"+1}c\""));
	Printf.printf "%s\n" (Syntax.show (parse_string "\"a\\{{{1+2}}}b\\{2}c\""));
	Printf.printf "%s\n" (Syntax.show (parse_string "\"{}\""));
