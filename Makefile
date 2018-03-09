test: Tests
	./Tests

Tests: Klotski.ml Tests.ml
	ocamlfind ocamlopt -o $@ -package graphics -linkpkg -package oUnit -linkpkg $^
