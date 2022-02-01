all:
	dune build
#	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest
install:
	dune install

#pad: the version.ml hack is ugly but I don't know better
# this seems the only issue with using semgrep as a submodule
setup:
	git submodule update --init --recursive
	cp src/cli/version.ml lang_GENERIC/semgrep/semgrep-core/src/cli/

update:
	git pull
	git submodule update
	@echo you may want to git pull also in lang_GENERIC/semgrep

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: all clean install test dump
