let parse_string s =
	Lexer.braces := [];
	(Parser.expression Lexer.token (Lexing.from_string s))

let test_string_literal() =
	assert(parse_string "\"string\"" = 1);
	(* escape sequence *)
	assert(parse_string "\"\\0\"" = 1);
	assert(parse_string "\"\\n\"" = 1);
	assert(parse_string "\"\\t\"" = 1);
	assert(parse_string "\"\\r\"" = 1);
	assert(parse_string "\"\\a\"" = 1);
	assert(parse_string "\"\\\\\"" = 1);
	(* string_expression *)
	assert(parse_string "\"ab\\{1+2}cc\"" = 1);
	assert(parse_string "\"ab\\{1+\"aaa\"+2}cc\"" = 1);
	assert(parse_string "\"ab\\{1+2}cd\\{3+4}de\"" = 1);
	()
let test_charactor_literal() =
	assert(parse_string "'a'" = 1);
	assert(parse_string "'1'" = 1);
	assert(parse_string "'0'" = 1);
	assert(parse_string "'\\0'" = 1);
	assert(parse_string "'\\n'" = 1);
	assert(parse_string "'\\t'" = 1);
	assert(parse_string "'\\r'" = 1);
	assert(parse_string "'\\a'" = 1);
	assert(parse_string "'\\\\'" = 1);
	()
let test_literal () =
	test_string_literal();
	test_charactor_literal();
	assert(parse_string "1" = 1);
	assert(parse_string "-1" = 1);
	assert(parse_string "+1" = 1);
	assert(parse_string "1y" = 1);
	assert(parse_string "1s" = 1);
	assert(parse_string "1l" = 1);
	assert(parse_string "1u" = 1);
	assert(parse_string "1uy" = 1);
	assert(parse_string "1us" = 1);
	assert(parse_string "1ul" = 1);
	assert(parse_string "1.1" = 1);
	assert(parse_string "1.11" = 1);
	assert(parse_string "1.1f" = 1);
	assert(parse_string "1.11f" = 1);
	assert(parse_string "1.11F" = 1);
	assert(parse_string "0" = 1);
	assert(parse_string "00" = 1);
	assert(parse_string "0777" = 1);
	assert(parse_string "0777ul" = 1);
	assert(parse_string "0x0" = 1);
	assert(parse_string "0xF" = 1);
	assert(parse_string "0x000000" = 1);
	assert(parse_string "0xFFFFFF" = 1);
	assert(parse_string "0xFFFFFFul" = 1);
	assert(parse_string "true" = 1);
	assert(parse_string "false" = 1);
	assert(parse_string "null" = 1);
	()
let test_implements () =
	assert(parse_string "1 implements abc" = 1);
	assert(parse_string "1 implements abc implements ddd" = 1);
	()
let test_mul () =
	assert(parse_string "1 * 1 * 1" = 1);
	assert(parse_string "1 / 1 / 1" = 1);
	assert(parse_string "1 % 1 % 1" = 1);
	assert(parse_string "1 * 1 / 1 % 1" = 1);
	()
let test_add () =
	assert(parse_string "1 + 1 + 1" = 1);
	assert(parse_string "1 - 1 - 1" = 1);
	assert(parse_string "1 + 1 - 1" = 1);
	()
let test_shift () =
	assert(parse_string "1 << 1 << 1" = 1);
	assert(parse_string "1 >> 1 >> 1" = 1);
	assert(parse_string "1 << 1 >> 1" = 1);
	()
let test_comparison () =
	assert(parse_string "1 <= 1 < 1" = 1);
	assert(parse_string "1 >= 1 > 1" = 1);
	()
let test_eq () =
	assert(parse_string "1 == 1 == 1" = 1);
	assert(parse_string "1 != 1 != 1" = 1);
	assert(parse_string "1 != 1 == 1" = 1);
	()
let test_or () =
	assert(parse_string "1 & 1 & 1" = 1);
	assert(parse_string "1 ^ 1 ^ 1" = 1);
	assert(parse_string "1 | 1 | 1" = 1);
	()
let test_lor () =
	assert(parse_string "1 || 1 || 1" = 1);
	assert(parse_string "1 && 1 && 1" = 1);
	()
let test_conditional() =
	assert(parse_string "1 ? 1 : 1" = 1);
	assert(parse_string "1.1 ? 1 : 1" = 1);
	assert(parse_string "1.1f ? 1 : 1" = 1);
	assert(parse_string "1 ? 1 : 1 ? 1 : 1" = 1);
	()
let _ =
	test_literal();
	(*test_implements();*)
	test_mul();
	test_add();
	test_shift();
	test_comparison();
	test_eq();
	test_or();
	test_lor();
	test_conditional();
	Printf.printf "ok\n"
