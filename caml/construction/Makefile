all: triangulations

remake: CLEAN dep all

modules:= boites triangulations 

cmo:=${modules:%=%.cmo}

triangulations: ${cmo}
	ocamlc -o final ${cmo}$

dep: 
	ocamldep *.mli *.ml > .depend
include .depend


clean:
	rm -f *.cm[iox] *~ .*~

CLEAN: clean
	rm -f final

%.cmi: %.mli
	ocamlc -c $<
%.cmo: %.ml
	ocamlc -c $<

