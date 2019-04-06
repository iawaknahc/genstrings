.PHONY: release setup

release:
	dune build -p genstrings

setup:
	opam switch create . 4.07.1 --deps-only
	opam install utop ocamlformat
