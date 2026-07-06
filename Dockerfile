FROM ocaml/opam:debian-13-ocaml-5.5
RUN sudo ln -sf /usr/bin/opam-2.5 /usr/bin/opam
# Ensure opam-repository is up-to-date:
RUN cd opam-repository && git pull -q origin 68acdc87b95f10206b36751f535034556b3cb606 && opam update
# Install utop for interactive use:
RUN opam install utop fmt
# Install Eio's dependencies (adding just the opam files first to help with caching):
RUN mkdir eio
WORKDIR eio
COPY *.opam ./
RUN opam pin --with-version=dev . -yn
RUN opam install --deps-only eio && opam install uring iomux yojson
# Build Eio:
COPY . ./
RUN opam install eio_main
