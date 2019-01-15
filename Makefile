.PHONY: all
all: homtrs

homtrs: freeModule.ml util.ml homcomp.ml trs_parse.mli trs_parse.ml trs_lex.ml farith.ml matrix.ml smith.ml reader.ml main.ml
	ocamlfind ocamlopt -linkpkg -package zarith freeModule.ml util.ml homcomp.ml trs_parse.mli trs_parse.ml trs_lex.ml farith.ml matrix.ml smith.ml reader.ml main.ml -o homtrs

trs_parse.mli trs_parse.ml: trs_parse.mly
	ocamlyacc trs_parse.mly

trs_lex.ml: trs_lex.mll
	ocamllex trs_lex.mll
