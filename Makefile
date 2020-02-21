MODULES=game command state main authors gui
MODULESK=game command state authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULESK:=.mli)
TEST=test.byte
MAIN=gui_main.byte
CMD=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,str,ANSITerminal,Images


default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

check:
	bash checkenv.sh && bash checktypes.sh
	
test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

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

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

cmd:
	$(OCAMLBUILD) $(CMD) && ./$(CMD)



clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private a6src.zip report bisect*.out
