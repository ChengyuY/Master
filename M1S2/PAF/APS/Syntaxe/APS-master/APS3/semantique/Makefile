LEX_ML = ocamllex
YACC_ML = /usr/local/bin/ocamlyacc
OCAMLC = ocamlc

all: evaluer clean 
test: parser test.ml
	$(OCAMLC) -o test ast.cmo lexer.cmo parser.cmo test.ml

evaluer: parser semantique.ml
	$(OCAMLC) -o evaluer ast.cmo lexer.cmo parser.cmo semantique.ml
parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml

