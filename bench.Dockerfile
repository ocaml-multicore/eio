FROM ocaml/opam:debian-11-ocaml-5.2
# Make sure we're using opam-2.1:
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
# Ensure opam-repository is up-to-date:
RUN cd opam-repository && git pull -q origin 97de3378749cf8d2d70a5d710d310e5cc17c9dea && opam update
# Install Eio's dependencies (adding just the opam files first to help with caching):
RUN mkdir eio
WORKDIR eio
COPY *.opam ./
RUN opam pin --with-version=dev . -yn
RUN opam install eio_main yojson
# Build the benchmarks:
COPY . ./
RUN opam exec -- dune build ./bench
