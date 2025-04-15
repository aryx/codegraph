
# TODO: add {,test.bc} at some point
default:
	dune build _build/install/default/bin/{codegraph,codegraph_build}

clean:
	dune clean
test:
	dune runtest
install:
	dune install

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: all clean install test dump
