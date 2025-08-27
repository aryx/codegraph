# Build codegraph (and semgrep and codemap) with OCaml 4.14.2 via OPAM on Ubuntu.
#

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
RUN opam init --disable-sandboxing -y # (disable sandboxing due to Docker)
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v


# Install semgrep libs (and its many dependencies) for codegraph_build
WORKDIR /semgrep
RUN git clone --depth=1 --recurse-submodules https://github.com/aryx/semgrep-libs /semgrep
#coupling: https://github.com/aryx/semgrep-libs/blob/master/Dockerfile
# and install-deps-UBUNTU-for-semgrep-core Makefile target
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
RUN ./configure
RUN eval $(opam env) && make && make dune-build-all
RUN eval $(opam env) && dune install
#TODO: can't because then can't find -ltree-sitter
# RUN rm -rf /semgrep

# Install codemap libs for codegraph (see list below after dune install)
WORKDIR /codemap
RUN apt-get install -y libcairo2-dev libgtk2.0-dev
# alt: add codemap as a submodule in codegraph source
RUN git clone --depth=1 https://github.com/aryx/codemap /codemap
RUN ./configure
RUN eval $(opam env) && make && make all
RUN eval $(opam env) && dune install \
    commons2_ files-format \
    visualization \
    graph_code highlight_code database_code layer_code \
    parser_c
RUN rm -rf /codemap


# Back to codegraph
WORKDIR /src

# Install other dependencies
COPY codegraph.opam configure ./
RUN ./configure

# Now let's build from source
COPY . .

RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && dune install

# Test
RUN eval $(opam env) && codegraph --help && codegraph_build --help
# TODO run more tests, make test!
