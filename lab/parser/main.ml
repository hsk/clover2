let parse_string s =
	Lexer.reset();
	(Parser.expression Lexer.token (Lexing.from_string s))

let test_collection() =
	assert(parse_string "list{}" = 1);
	assert(parse_string "equalable_list{}" = 1);
	assert(parse_string "elist{}" = 1);
	assert(parse_string "array{}" = 1);
	assert(parse_string "equalable_array{}" = 1);
	assert(parse_string "earray{}" = 1);
	assert(parse_string "tuple{}" = 1);
	assert(parse_string "[]" = 1);

	assert(parse_string "list{1}" = 1);
	assert(parse_string "equalable_list{1}" = 1);
	assert(parse_string "elist{1}" = 1);
	assert(parse_string "array{1}" = 1);
	assert(parse_string "equalable_array{1}" = 1);
	assert(parse_string "earray{1}" = 1);
	assert(parse_string "tuple{1}" = 1);
	assert(parse_string "[1]" = 1);

	assert(parse_string "list{1,1+2}" = 1);
	assert(parse_string "equalable_list{1,1+2}" = 1);
	assert(parse_string "elist{1,1+2}" = 1);
	assert(parse_string "array{1,1+2}" = 1);
	assert(parse_string "equalable_array{1,1+2}" = 1);
	assert(parse_string "earray{1,1+2}" = 1);
	assert(parse_string "tuple{1,1+2}" = 1);
	assert(parse_string "[1,1+2]" = 1);

	assert(parse_string "list{1,1+2,3}" = 1);
	assert(parse_string "equalable_list{1,1+2,3}" = 1);
	assert(parse_string "elist{1,1+2,3}" = 1);
	assert(parse_string "array{1,1+2,3}" = 1);
	assert(parse_string "equalable_array{1,1+2,3}" = 1);
	assert(parse_string "earray{1,1+2,3}" = 1);
	assert(parse_string "tuple{1,1+2,3}" = 1);
	assert(parse_string "[1,1+2,3]" = 1);

	assert(parse_string "hash{}" = 1);
	assert(parse_string "hash{1:1}" = 1);
	assert(parse_string "hash{1:1,2:1+2}" = 1);
	assert(parse_string "hash{\"1\":1,\"2\":1+2,\"3\":3}" = 1);

	()
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
let test_pstring_literal() =
	assert(parse_string "p\"string\"" = 1);
	assert(parse_string "P\"string\"" = 1);
	(* escape sequence *)
	assert(parse_string "p\"\\0\"" = 1);
	assert(parse_string "p\"\\n\"" = 1);
	assert(parse_string "p\"\\t\"" = 1);
	assert(parse_string "p\"\\r\"" = 1);
	assert(parse_string "p\"\\a\"" = 1);
	assert(parse_string "p\"\\\\\"" = 1);
	(* string_expression *)
	assert(parse_string "p\"ab\\{1+2}cc\"" = 1);
	assert(parse_string "p\"ab\\{1+b\"aaa\"+2}cc\"" = 1);
	assert(parse_string "p\"ab\\{1+2}cd\\{3+4}de\"" = 1);
	()
let test_bstring_literal() =
	assert(parse_string "b\"string\"" = 1);
	assert(parse_string "B\"string\"" = 1);
	(* escape sequence *)
	assert(parse_string "b\"\\0\"" = 1);
	assert(parse_string "b\"\\n\"" = 1);
	assert(parse_string "b\"\\t\"" = 1);
	assert(parse_string "b\"\\r\"" = 1);
	assert(parse_string "b\"\\a\"" = 1);
	assert(parse_string "b\"\\\\\"" = 1);
	(* string_expression *)
	assert(parse_string "b\"ab\\{1+2}cc\"" = 1);
	assert(parse_string "b\"ab\\{1+b\"aaa\"+2}cc\"" = 1);
	assert(parse_string "b\"ab\\{1+2}cd\\{3+4}de\"" = 1);
	()

	let test_regexp_literal() =
		assert(parse_string "/string/" = 1);
		(* escape sequence *)
		assert(parse_string "/\\0/" = 1);
		assert(parse_string "/\\n/" = 1);
		assert(parse_string "/\\t/" = 1);
		assert(parse_string "/\\r/" = 1);
		assert(parse_string "/\\a/" = 1);
		assert(parse_string "/\\\\/" = 1);
		assert(parse_string "/{a}/" = 1);
		
		(* string_expression *)
		assert(parse_string "/ab\\{1+2}cc/" = 1);
		assert(parse_string "/ab\\{1+/aaa/+2}cc/" = 1);
		assert(parse_string "/ab\\{1+2}cd\\{\"3\"+4}de/" = 1);

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
	test_pstring_literal();
	test_bstring_literal();
	test_charactor_literal();
	test_regexp_literal();
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
let test_assign_expression() =
	assert(parse_string "abc := 1" = 1);
	assert(parse_string "../abc : int = 1" = 1);
	assert(parse_string "../abc += 1" = 1);
	assert(parse_string "../abc -= 1" = 1);
	assert(parse_string "../abc *= 1" = 1);
	assert(parse_string "../abc /= 1" = 1);
	assert(parse_string "../abc <<= 1" = 1);
	assert(parse_string "../abc >>= 1" = 1);
	assert(parse_string "../abc &= 1" = 1);
	assert(parse_string "../abc ^= 1" = 1);
	assert(parse_string "../abc |= 1" = 1);
	assert(parse_string "& abc" = 1);
	()
let test_method_expression() =
	assert(parse_string "abc int.a()" = 1);
	assert(parse_string "abc int.a(){}" = 1);
	assert(parse_string "abc int.a(){| |  }" = 1);
	assert(parse_string "abc int.a(){|a:int | a+1 }" = 1);
	assert(parse_string "abc int.a(){|a:int,b:int |:int a+b }" = 1);
	assert(parse_string "abc int.a == 1" = 1);
	assert(parse_string "abc int.a += 1" = 1);
	assert(parse_string "a()" = 1);
	assert(parse_string "a(1 @abc)" = 1);
	()
let test_postposition_operator () = 
	assert(parse_string "a..a" = 1);
	assert(parse_string "a..a(1,2,3)" = 1);
	assert(parse_string "a..a == 1" = 1);
	assert(parse_string "a..a += 1" = 1);
	assert(parse_string "a[1] == 1" = 1);
	assert(parse_string "a[1] += 1" = 1);
	
	assert(parse_string "a->a" = 1);
	assert(parse_string "a->a == 1" = 1);
	assert(parse_string "a++" = 1);
	assert(parse_string "a-- ++" = 1);
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
let test_control() =
	assert(parse_string "{}" = 1);
	assert(parse_string "{1}" = 1);
	assert(parse_string "{1;2}" = 1);
	assert(parse_string "{1;2;}" = 1);
	assert(parse_string "if(1){}" = 1);
	assert(parse_string "if(1){}else{}" = 1);
	assert(parse_string "if(1){}elif(1){}" = 1);
	assert(parse_string "if(1){}elif(1){}else{}" = 1);
	assert(parse_string "if(1){}elif(1){}elif(1){}else{}" = 1);
	assert(parse_string "while(1){break}" = 1);
	assert(parse_string "for(1;1;1){return}" = 1);
	assert(parse_string "for(1;1;1){return return 1}" = 1);
	assert(parse_string "throw 1" = 1);
	assert(parse_string "try { throw 1} catch (a:int) { 1 } " = 1);
	(*assert(parse_string "try { throw 1} catch (a:int) { 1 } catch (a:obj) { 1 } " = 1);*)
	assert(parse_string "lambda () {}" = 1);
	assert(parse_string "closure () {}" = 1);
	assert(parse_string "lambda ():int {}" = 1);
	assert(parse_string "closure ():int {}" = 1);
	assert(parse_string "lambda (a:int,b:int):int {}" = 1);
	assert(parse_string "closure (a:int,b:int):int {}" = 1);
	assert(parse_string "def a(a:int,b:int):int {}" = 1);
	assert(parse_string "inherit ()" = 1);
	assert(parse_string "inherit (1 @a,2@b)" = 1);
	()
let _ =
	test_collection();
	test_literal();
	test_assign_expression();
	test_method_expression();
	test_postposition_operator();
	(*test_implements();*)
	test_mul();
	test_add();
	test_shift();
	test_comparison();
	test_eq();
	test_or();
	test_lor();
	test_conditional();
	test_control();
	Printf.printf "ok\n"
