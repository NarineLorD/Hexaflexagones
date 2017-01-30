#Makefile pour l'interface graphique du programme de flexagones
labltk.cma!: 
	ocamlfind -I +labltk


interface_tk::  labltk.cma
	ocamlfind ocamlc -oI lanceur +labltk labltk.cma
