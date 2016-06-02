OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = rpclib,lwt,lwt.unix,result
EXAMPLES = \
	all_types \
	xapi \
	json \
	option \
	encoding \
	dict \
	variants 
#	client \
#	client_lwt \
#	phantom

EXECS=$(foreach example, $(EXAMPLES), $(example).opt)

.PHONY: all clean
all: $(EXECS)
	make $(EXECS:%=%.run)

%.run: %
	./$?

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),ppx_deriving_rpc -c -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),ppx_deriving_rpc -c -o $@ $<

%_gen: %.ml
	camlp4o $(shell ocamlfind query rpclib.syntax -r -format "-I %d %a" -predicates syntax,preprocessor) $< -printer o > $@.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) -c -o $@ $@.ml

idl_test_gen.ml: idl_test.ml
	camlp4o $(shell ocamlfind query rpclib.idl -r -format "-I %d %a" -predicates syntax,preprocessor) $< -printer o > $@


clean:
	rm -f *_gen.ml *.annot *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)
