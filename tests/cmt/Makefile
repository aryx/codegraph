#TODO: why need --root? There is a dune-project here so dune should
# not need to go in the parent directory

all:
	dune build --root .

test: all
	codegraph_build -lang cmt .

clean:
	dune clean --root .
