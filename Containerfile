FROM registry.fedoraproject.org/fedora:35
RUN dnf upgrade -y && dnf install -y opam ispc parallel xonsh procps git && dnf clean all
RUN opam init --disable-sandboxing -a -c 4.13.1+options && opam install -y ocaml-option-flambda

COPY staged-synth.opam /tmp
RUN opam install -y --deps-only /tmp/staged-synth.opam
CMD /bin/bash -i