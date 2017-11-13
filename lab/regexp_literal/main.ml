let parse_string s =
	Lexer.reset();
	(Parser.exp Lexer.token (Lexing.from_string s))

let _ =
	Printf.printf "%s\n" (Syntax.show (parse_string "1"));
	Printf.printf "%s\n" (Syntax.show (parse_string "1*2+3/4/2"));
	Printf.printf "%s\n" (Syntax.show (parse_string "1*2+(3/4)/2"));
	Printf.printf "%s\n" (Syntax.show (parse_string "/1*2/"));
	Printf.printf "%s\n" (Syntax.show (parse_string "/1*2/ / 5"));
	Printf.printf "%s\n" (Syntax.show (parse_string "/1*2/ / /1*2/"));
	Printf.printf "%s\n" (Syntax.show (parse_string "//aaa\n (/1*2/) / (/1*2/)"));
	Printf.printf "%s\n" (Syntax.show (parse_string "//\n (/1*2/) / (/1*2/)"));
	Printf.printf "%s\n" (Syntax.show (parse_string "/**/ //\n (/1*2/) /= (/1*2/)"));
