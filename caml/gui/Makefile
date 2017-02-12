#Makefile pour l'interface graphique du programme 
all: lanceur

lanceur:
	ocamlc -o  lanceur -I +labltk labltk.cma lanceur.ml

%.cmo: %.ml
	ocamlc -c $<


%.cmi: %.mli
	ocamlc -c $<



clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt
	rm -f *.o
