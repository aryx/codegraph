all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest
install:
	dune install

setup:
	git submodule update --init --recursive

.PHONY: all clean install test dump
