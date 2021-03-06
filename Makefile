PROGRAM_MODULES=quiz progress main builder validation
TEST_MODULES=testUtils testQuiz testProgress testValidation test
MODULES=$(PROGRAM_MODULES) $(TEST_MODULES)
OBJECTS=$(MODULES:=.cmo)
MLS=$(PROGRAM_MODULES:=.ml)
MLIS=$(PROGRAM_MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,qcheck,ansiterminal,yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

bisect-test:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

check:
	bash checkenv.sh && bash checktypes.sh

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
	
finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

bisect: clean bisect-test
	bisect-report -I _build -html report bisect0001.out

zip:
	zip askyquiz_src.zip *.ml* _tags Makefile *.quiz quizzes/*.quiz README.md

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report askyquiz_src.zip bisect*.out
