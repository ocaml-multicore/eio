FROM ocaml/opam:debian-11-ocaml-5.0
# Make sure we're using opam-2.1:
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
# Ensure opam-repository is up-to-date:
RUN cd opam-repository && git pull -q origin 2b82c83694d8f1db63e504823646ffcb51562f75 && opam update
# Switch to OCaml 5.0.0 (beta)
RUN opam pin remove ocaml-variants && opam install ocaml-base-compiler.5.0.0~beta2 --update-invariant
# Install utop for interactive use:
RUN opam install utop fmt
# Install Eio's dependencies (adding just the opam files first to help with caching):
RUN mkdir eio
WORKDIR eio
COPY *.opam ./
RUN opam install --deps-only .
# Build Eio:
COPY . ./
RUN opam install .
