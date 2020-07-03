MAIN=ucdsltest

OBJS = ast.cmo  uclexer.cmo ucparser.cmo  main.cmo

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


$(MAIN): clean $(OBJS)
	ocamlc -g -o $(MAIN) unix.cma str.cma $(OBJS) 

uclexer.ml : uclexer.mll
	ocamllex -q $<

uclexer.cmo : ucparser.cmi uclexer.ml
	ocamlc -g -c uclexer.ml

ucparser.ml : ucparser.mly
	menhir $<

ucparser.mli : ucparser.mly
	menhir $<

clean:
	rm -f *.cmo *.cmi uclexer.ml ucparser.ml ucparser.mli $(MAIN)
