#Makefile pour l'interface graphique du programme 
all: interface

remake: CLEAN all


modules =  module interface
cmo:=${modules:%=%.cmo}

interface: ${cmo}
	ocamlc -o interface -I +labltk labltk.cma ${cmo}

%.cmo: %.ml
	ocamlc -c -I +labltk labltk.cma $<
%.cmi: %.mli
	ocamlc -c $<

dep: 
	ocamldep *.mli *.ml > .depend
include .depend

clean: 
	rm -f *.cm[iox] *~ .*~ \#*\# *.o

CLEAN: clean
	rm -f interface
