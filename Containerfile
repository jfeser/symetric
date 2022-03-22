FROM registry.fedoraproject.org/fedora:35
RUN dnf upgrade -y && dnf install -y opam ispc parallel xonsh procps gcc-c++ openblas-devel zlib-devel
RUN opam init --disable-sandboxing -a && opam switch create 4.13.1+options && opam install -y ocaml-option-flambda

COPY staged-synth.opam /tmp
RUN opam install -y --deps-only /tmp/staged-synth.opam
