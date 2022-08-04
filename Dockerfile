FROM ocaml/opam:debian-11-ocaml-5.0
# Make sure we're using opam-2.1:
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
# Add the alpha repository with some required preview versions of dependencies:
RUN opam remote add alpha git+https://github.com/kit-ty-kate/opam-alpha-repository.git
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
