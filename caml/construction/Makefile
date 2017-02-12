all: build_flexagons

ALL: clean dep all clean

build_flexagons  flexagones.cmo flexagones.cmi: 
	ocamlc -o final build_flexagons.ml

dep: 
	ocamldep *.mli *.ml > .depend
include .depend
clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f final.opt


%.cmi: %.mli
	ocamlc -c $<
%.cmo: %.ml
	ocamlc -c $<
