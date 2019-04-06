.PHONY: clean release setup fmt

release:
	dune build -p genstrings

clean:
	dune clean

setup:
	opam switch create . 4.07.1 --deps-only
	opam install utop ocamlformat

fmt:
	@dune build --auto-promote @fmt
