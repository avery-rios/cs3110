check:
	bash checkenv.sh && bash checktypes.sh

admitted:
	coqc a5.v
	coqchk -silent -o -norec a5

clean:
	rm -f *.vo *.glob
