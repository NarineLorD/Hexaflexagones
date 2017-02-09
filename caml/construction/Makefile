all: build_flexagons

ALL: clean dep all clean
cmo = flexagones.cmo arbresb.cmo
cmi = flexagones.cmi arbresb.cmi
build_flexagons: $(cmi) $(cmo) 
	ocamlc -o final build_flexagons.ml

flexagones: $(cmi) $(cmo)
	ocamlc flexagones.ml

dep: 
	ocamldep *.mli *.ml > .depend

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f final
	rm -f final.opt

include .depend

%.cmi: %.mli
	ocamlc  $<
%.cmo: %.ml
	ocamlc  $<
