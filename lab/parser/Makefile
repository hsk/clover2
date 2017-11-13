all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamlc parser.ml lexer.ml main.ml -o test
	rm -rf *.cm* parser.ml lexer.ml
	./test
clean:
	rm -rf *.cm* parser.ml lexer.ml test
