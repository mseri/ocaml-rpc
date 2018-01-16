PREFIX = -I ../_build/default

OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLFLAGS = -annot -g

PACKS = unix,rresult,xmlm,yojson,rpclib,rpclib.json,rpclib.xml

RPC = -I $(PREFIX)/camlp4/idl -I $(PREFIX)/camlp4/syntax
CAMLP4 = camlp4o $(shell ocamlfind query type_conv -r -format "-I %d %a" -predicates syntax,preprocessor) $(RPC) rpclib_syntax.cma

EXAMPLES = \
	all_types \
	phantom \
	option \
	encoding \
	dict \
	variants \
	client \
	client_lwt

EXECS=$(foreach example, $(EXAMPLES), $(example).opt)

.PHONY: all clean
all: $(EXECS)
	make $(EXECS:%=%.run)

%.run: %
	./$?

client.opt: idl_test.cmx idl_test.cmi client.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) $(RPC) -o $@ idl_test.cmx client.cmx

client_lwt.opt: idl_test.cmx idl_test.cmi client_lwt.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS),rpclib.lwt,lwt,lwt.unix $(RPC) -o $@ idl_test.cmx client_lwt.cmx

idl_test.cmx: idl_test.ml idl_test.cmi
	$(OCAMLOPT) -package $(PACKS) $(RPC) idl.cmxa -pp "camlp4o $(RPC) idl.cma" -c -o $@ $<

%.opt: %.cmx
	$(OCAMLOPT) -linkpkg -package $(PACKS) $(RPC) -o $@ $<

client_lwt.cmx: client_lwt.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS),rpclib.lwt,lwt,lwt.unix $(RPC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) $(RPC) -pp "$(CAMLP4)" -c -o $@ $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) $(RPC) -pp "$(CAMLP4)" -c -o $@ $<

%_gen: %.ml
	$(CAMLP4) $< -printer o > $@.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -package $(PACKS) $(RPC) -c -o $@ $@.ml

idl_test_gen.ml: idl_test.ml
	camlp4o $(RPC) idl.cma  $< -printer o > $@


clean:
	rm -f *_gen.ml *.annot *.cmx *.cmi *.cmo *.cmxa *.o $(EXECS)
