let parse s =
	Lexer.reset();
	(Parser.expression Lexer.token (Lexing.from_string s))

let parse_type s =
	Lexer.reset();
	(Parser.type_ Lexer.token (Lexing.from_string s))
let test_type() =
	assert(parse_type "int" = 1);
	assert(parse_type "lambda()" = 1);
	assert(parse_type "lambda():int" = 1);
	assert(parse_type "lambda(int)" = 1);
	assert(parse_type "lambda(int):int" = 1);
	assert(parse_type "lambda(int) @anon" = 1);
	assert(parse_type "lambda(int):int @anon" = 1);
	assert(parse_type "lambda(int)[]" = 1);
	assert(parse_type "lambda(int):int[]" = 1);
	assert(parse_type "lambda(int) []@anon" = 1);
	assert(parse_type "lambda(int):int[] @anon" = 1);
	assert(parse_type "lambda" = 1);
	assert(parse_type "lambda:int" = 1);
	assert(parse_type "lambda @anon" = 1);
	assert(parse_type "lambda:int @anon" = 1);
	assert(parse_type "lambda[]" = 1);
	assert(parse_type "lambda:int[]" = 1);
	assert(parse_type "lambda []@anon" = 1);
	assert(parse_type "lambda:int[] @anon" = 1);
	assert(parse_type "A" = 1);
	assert(parse_type "A<int>" = 1);
	assert(parse_type "A[]" = 1);
	assert(parse_type "A<int>[]" = 1);
	assert(parse_type "A @aa" = 1);
	assert(parse_type "A<int>@aa" = 1);
	assert(parse_type "A[]@aa" = 1);
	assert(parse_type "A<int>[]@aa" = 1);
	()
let test_collection() =
	assert(parse "list{}" = 1);
	assert(parse "equalable_list{}" = 1);
	assert(parse "elist{}" = 1);
	assert(parse "array{}" = 1);
	assert(parse "equalable_array{}" = 1);
	assert(parse "earray{}" = 1);
	assert(parse "tuple{}" = 1);
	assert(parse "[]" = 1);

	assert(parse "list{1}" = 1);
	assert(parse "equalable_list{1}" = 1);
	assert(parse "elist{1}" = 1);
	assert(parse "array{1}" = 1);
	assert(parse "equalable_array{1}" = 1);
	assert(parse "earray{1}" = 1);
	assert(parse "tuple{1}" = 1);
	assert(parse "[1]" = 1);

	assert(parse "list{1,1+2}" = 1);
	assert(parse "equalable_list{1,1+2}" = 1);
	assert(parse "elist{1,1+2}" = 1);
	assert(parse "array{1,1+2}" = 1);
	assert(parse "equalable_array{1,1+2}" = 1);
	assert(parse "earray{1,1+2}" = 1);
	assert(parse "tuple{1,1+2}" = 1);
	assert(parse "[1,1+2]" = 1);

	assert(parse "list{1,1+2,3}" = 1);
	assert(parse "equalable_list{1,1+2,3}" = 1);
	assert(parse "elist{1,1+2,3}" = 1);
	assert(parse "array{1,1+2,3}" = 1);
	assert(parse "equalable_array{1,1+2,3}" = 1);
	assert(parse "earray{1,1+2,3}" = 1);
	assert(parse "tuple{1,1+2,3}" = 1);
	assert(parse "[1,1+2,3]" = 1);

	assert(parse "hash{}" = 1);
	assert(parse "hash{1:1}" = 1);
	assert(parse "hash{1:1,2:1+2}" = 1);
	assert(parse "hash{\"1\":1,\"2\":1+2,\"3\":3}" = 1);

	()
let test_string_literal() =
	assert(parse "\"string\"" = 1);
	(* escape sequence *)
	assert(parse "\"\\0\"" = 1);
	assert(parse "\"\\n\"" = 1);
	assert(parse "\"\\t\"" = 1);
	assert(parse "\"\\r\"" = 1);
	assert(parse "\"\\a\"" = 1);
	assert(parse "\"\\\\\"" = 1);
	(* string_expression *)
	assert(parse "\"ab\\{1+2}cc\"" = 1);
	assert(parse "\"ab\\{1+\"aaa\"+2}cc\"" = 1);
	assert(parse "\"ab\\{1+2}cd\\{3+4}de\"" = 1);
	()
let test_pstring_literal() =
	assert(parse "p\"string\"" = 1);
	assert(parse "P\"string\"" = 1);
	(* escape sequence *)
	assert(parse "p\"\\0\"" = 1);
	assert(parse "p\"\\n\"" = 1);
	assert(parse "p\"\\t\"" = 1);
	assert(parse "p\"\\r\"" = 1);
	assert(parse "p\"\\a\"" = 1);
	assert(parse "p\"\\\\\"" = 1);
	(* string_expression *)
	assert(parse "p\"ab\\{1+2}cc\"" = 1);
	assert(parse "p\"ab\\{1+b\"aaa\"+2}cc\"" = 1);
	assert(parse "p\"ab\\{1+2}cd\\{3+4}de\"" = 1);
	()
let test_bstring_literal() =
	assert(parse "b\"string\"" = 1);
	assert(parse "B\"string\"" = 1);
	(* escape sequence *)
	assert(parse "b\"\\0\"" = 1);
	assert(parse "b\"\\n\"" = 1);
	assert(parse "b\"\\t\"" = 1);
	assert(parse "b\"\\r\"" = 1);
	assert(parse "b\"\\a\"" = 1);
	assert(parse "b\"\\\\\"" = 1);
	(* string_expression *)
	assert(parse "b\"ab\\{1+2}cc\"" = 1);
	assert(parse "b\"ab\\{1+b\"aaa\"+2}cc\"" = 1);
	assert(parse "b\"ab\\{1+2}cd\\{3+4}de\"" = 1);
	()

	let test_regexp_literal() =
		assert(parse "/string/" = 1);
		(* escape sequence *)
		assert(parse "/\\0/" = 1);
		assert(parse "/\\n/" = 1);
		assert(parse "/\\t/" = 1);
		assert(parse "/\\r/" = 1);
		assert(parse "/\\a/" = 1);
		assert(parse "/\\\\/" = 1);
		assert(parse "/{a}/" = 1);
		
		(* string_expression *)
		assert(parse "/ab\\{1+2}cc/" = 1);
		assert(parse "/ab\\{1+/aaa/+2}cc/" = 1);
		assert(parse "/ab\\{1+2}cd\\{\"3\"+4}de/" = 1);

		()
	
let test_charactor_literal() =
	assert(parse "'a'" = 1);
	assert(parse "'1'" = 1);
	assert(parse "'0'" = 1);
	assert(parse "'\\0'" = 1);
	assert(parse "'\\n'" = 1);
	assert(parse "'\\t'" = 1);
	assert(parse "'\\r'" = 1);
	assert(parse "'\\a'" = 1);
	assert(parse "'\\\\'" = 1);
	()
let test_literal () =
	test_string_literal();
	test_pstring_literal();
	test_bstring_literal();
	test_charactor_literal();
	test_regexp_literal();
	assert(parse "1" = 1);
	assert(parse "-1" = 1);
	assert(parse "+1" = 1);
	assert(parse "1y" = 1);
	assert(parse "1s" = 1);
	assert(parse "1l" = 1);
	assert(parse "1u" = 1);
	assert(parse "1uy" = 1);
	assert(parse "1us" = 1);
	assert(parse "1ul" = 1);
	assert(parse "1.1" = 1);
	assert(parse "1.11" = 1);
	assert(parse "1.1f" = 1);
	assert(parse "1.11f" = 1);
	assert(parse "1.11F" = 1);
	assert(parse "0" = 1);
	assert(parse "00" = 1);
	assert(parse "0777" = 1);
	assert(parse "0777ul" = 1);
	assert(parse "0x0" = 1);
	assert(parse "0xF" = 1);
	assert(parse "0x000000" = 1);
	assert(parse "0xFFFFFF" = 1);
	assert(parse "0xFFFFFFul" = 1);
	assert(parse "true" = 1);
	assert(parse "false" = 1);
	assert(parse "null" = 1);
	()
let test_assign_expression() =
	assert(parse "abc := 1" = 1);
	assert(parse "../abc : int = 1" = 1);
	assert(parse "../abc += 1" = 1);
	assert(parse "../abc -= 1" = 1);
	assert(parse "../abc *= 1" = 1);
	assert(parse "../abc /= 1" = 1);
	assert(parse "../abc <<= 1" = 1);
	assert(parse "../abc >>= 1" = 1);
	assert(parse "../abc &= 1" = 1);
	assert(parse "../abc ^= 1" = 1);
	assert(parse "../abc |= 1" = 1);
	assert(parse "& abc" = 1);
	()
let test_method_expression() =
	assert(parse "abc int.a()" = 1);
	assert(parse "abc int.a(){}" = 1);
	assert(parse "abc int.a(){| |  }" = 1);
	assert(parse "abc int.a(){|a:int | a+1 }" = 1);
	assert(parse "abc int.a(){|a:int,b:int |:int a+b }" = 1);
	assert(parse "abc int.a == 1" = 1);
	assert(parse "abc int.a += 1" = 1);
	assert(parse "a()" = 1);
	assert(parse "a(1 @abc)" = 1);
	()
let test_postposition_operator () = 
	assert(parse "a..a" = 1);
	assert(parse "a..a(1,2,3)" = 1);
	assert(parse "a..a == 1" = 1);
	assert(parse "a..a += 1" = 1);
	assert(parse "a[1] == 1" = 1);
	assert(parse "a[1] += 1" = 1);
	
	assert(parse "a->a" = 1);
	assert(parse "a->a == 1" = 1);
	assert(parse "a++" = 1);
	assert(parse "a-- ++" = 1);
	()
	
let test_implements () =
	assert(parse "1 implements abc" = 1);
	assert(parse "1 implements abc implements ddd" = 1);
	()
let test_mul () =
	assert(parse "1 * 1 * 1" = 1);
	assert(parse "1 / 1 / 1" = 1);
	assert(parse "1 % 1 % 1" = 1);
	assert(parse "1 * 1 / 1 % 1" = 1);
	()
let test_add () =
	assert(parse "1 + 1 + 1" = 1);
	assert(parse "1 - 1 - 1" = 1);
	assert(parse "1 + 1 - 1" = 1);
	()
let test_shift () =
	assert(parse "1 << 1 << 1" = 1);
	assert(parse "1 >> 1 >> 1" = 1);
	assert(parse "1 << 1 >> 1" = 1);
	()
let test_comparison () =
	assert(parse "1 <= 1 < 1" = 1);
	assert(parse "1 >= 1 > 1" = 1);
	()
let test_eq () =
	assert(parse "1 == 1 == 1" = 1);
	assert(parse "1 != 1 != 1" = 1);
	assert(parse "1 != 1 == 1" = 1);
	()
let test_or () =
	assert(parse "1 & 1 & 1" = 1);
	assert(parse "1 ^ 1 ^ 1" = 1);
	assert(parse "1 | 1 | 1" = 1);
	()
let test_lor () =
	assert(parse "1 || 1 || 1" = 1);
	assert(parse "1 && 1 && 1" = 1);
	()
let test_conditional() =
	assert(parse "1 ? 1 : 1" = 1);
	assert(parse "1.1 ? 1 : 1" = 1);
	assert(parse "1.1f ? 1 : 1" = 1);
	assert(parse "1 ? 1 : 1 ? 1 : 1" = 1);
	()
let test_control() =
	assert(parse "{}" = 1);
	assert(parse "{1}" = 1);
	assert(parse "{1;2}" = 1);
	assert(parse "{1;2;}" = 1);
	assert(parse "if(1){}" = 1);
	assert(parse "if(1){}else{}" = 1);
	assert(parse "if(1){}elif(1){}" = 1);
	assert(parse "if(1){}elif(1){}else{}" = 1);
	assert(parse "if(1){}elif(1){}elif(1){}else{}" = 1);
	assert(parse "while(1){break}" = 1);
	assert(parse "for(1;1;1){return}" = 1);
	assert(parse "for(1;1;1){return return 1}" = 1);
	assert(parse "throw 1" = 1);
	assert(parse "try { throw 1} catch (a:int) { 1 } " = 1);
	(*assert(parse "try { throw 1} catch (a:int) { 1 } catch (a:obj) { 1 } " = 1);*)
	assert(parse "lambda () {}" = 1);
	assert(parse "closure () {}" = 1);
	assert(parse "lambda ():int {}" = 1);
	assert(parse "closure ():int {}" = 1);
	assert(parse "lambda (a:int,b:int):int {}" = 1);
	assert(parse "closure (a:int,b:int):int {}" = 1);
	assert(parse "def a(a:int,b:int):int {}" = 1);
	assert(parse "def a(a:int,b:int):int<T> {}" = 1);
	assert(parse "def a(a:int,b:int):int<T,U> {}" = 1);
	assert(parse "inherit ()" = 1);
	assert(parse "inherit (1 @a,2@b)" = 1);
	()
let _ =
	test_type();
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
