###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

# TODO: add {,test.bc} at some point
default:
	bash -c "dune build _build/install/default/bin/{codegraph,codegraph_build}"

all:
	dune build
clean:
	dune clean
test:
	dune runtest
install:
	dune install

.PHONY: all clean test install

build-docker:
	docker build -t "codegraph" .

###############################################################################
# Developer targets
###############################################################################

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .
