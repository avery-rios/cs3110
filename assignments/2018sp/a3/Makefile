test:
	ocamlbuild -use-ocamlfind test_main.byte && ./test_main.byte

compile:
	ocamlbuild -use-ocamlfind data.cmo engine.cmo

check:
	bash checkenv.sh && bash checktypes.sh

zip:
	zip a3src.zip *.ml*
	
zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f a3src.zip
