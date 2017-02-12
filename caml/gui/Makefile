#Makefile pour l'interface graphique du programme 
all: lanceur


lanceur:
	ocamlc -o  interface -I +labltk labltk.cma interface.ml

%.cmo: %.ml
	ocamlc -c $<


%.cmi: %.mli
	ocamlc -c $<


clean: 
	rm -f *.cm[iox] *~ .*~ \#*\# *.o

CLEAN: clean
	rm -f interface
