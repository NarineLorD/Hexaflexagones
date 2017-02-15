#Makefile pour l'interface graphique du programme 
all: interface

remake: CLEAN all

interface: module.cmi module.cmo
	ocamlc -o  interface -I +labltk labltk.cma interface.ml

%.cmo: %.ml
	ocamlc -c $<


%.cmi: %.mli
	ocamlc -c $<


clean: 
	rm -f *.cm[iox] *~ .*~ \#*\# *.o

CLEAN: clean
	rm -f interface
