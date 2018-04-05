test: Tests
	./Tests

profile: Klotski_Profiling
	./$<
	gprof $<

PKGS = -package graphics -package unix -package oUnit -linkpkg

Tests: Klotski.ml Tests.ml
	ocamlfind ocamlopt -o $@ $(PKGS) $^

Klotski.opt: Klotski.ml
	ocamlfind ocamlopt -o $@ $(PKGS) $^

Klotski.byte: Klotski.ml
	ocamlfind ocamlc -o $@ $(PKGS) $^

Klotski.cmo: Klotski.ml
	ocamlfind ocamlc -c -o $@ $(PKGS) $^

Klotski_Solver:  Klotski.ml UseKlotski.ml
	ocamlfind ocamlopt -o $@ $(PKGS) $^

Klotski_Profiling:  Klotski.ml Profile_Klotski.ml
	ocamlfind ocamlopt -p -o $@ $(PKGS) $^
