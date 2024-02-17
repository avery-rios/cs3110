test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte

check:
	bash checkenv.sh && bash checktypes.sh

zip:
	zip a4src.zip *.ml*
	
zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f a4src.zip