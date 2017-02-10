#Makefile pour l'interface graphique du programme 
all: lanceur

lanceur:
	ocamlopt -c -I +labltk lanceur.ml

%.cmo: %.ml
	ocamlc -c $<


%.cmi: %.mli
	ocamlc -c $<
