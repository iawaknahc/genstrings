.PHONY: clean release fmt

release:
	dune build -p genstrings

clean:
	dune clean

fmt:
	@dune build --auto-promote @fmt
