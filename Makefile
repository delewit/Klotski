test: Tests
	./Tests

Tests: Klotski.ml Tests.ml
	ocamlfind ocamlopt -o $@ -package oUnit -linkpkg $^
