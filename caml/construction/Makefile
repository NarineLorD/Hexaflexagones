all: build_flexagons

remake: CLEAN dep all

modules:=arbresb flexagones build_flexagons

cmo:=${modules:%=%.cmo}

build_flexagons: ${cmo}
	ocamlc -o final ${cmo}


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

