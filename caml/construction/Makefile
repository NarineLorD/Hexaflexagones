%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<



build_flexagons: flexagones.cmo flexagones.cmi
		ocamlc -o build_flexagons build_flexagons.ml

flexagones: flexagones.ml flexagones.mli
