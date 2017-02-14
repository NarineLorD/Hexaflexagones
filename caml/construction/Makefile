
all: build_flexagons

ALL: clean dep all

build_flexagons: flexagones.cmi flexagones.cmo
	ocamlc -o final build_flexagons.ml


dep: 
	ocamldep *.mli *.ml > .depend
include .depend
clean:
	rm -f *.cm[iox] *~ .*~ #*#

CLEAN: clean
	rm -f final

%.cmi: %.mli
	ocamlc -c $<
%.cmo: %.ml
	ocamlc -c $<
