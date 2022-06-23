FROM registry.fedoraproject.org/fedora:36
RUN dnf upgrade -y && dnf install -y opam ispc parallel xonsh procps git perl-Time-HiRes && dnf clean all
RUN opam init --disable-sandboxing -a -c 4.14.0+options && opam install -y ocaml-option-flambda

ADD https://people.csail.mit.edu/asolar/sketch-1.7.6.tar.gz /tmp/sketch.tar.gz
RUN mkdir -p /opt/sketch && tar -x --no-same-owner --strip-components=1 -f /tmp/sketch.tar.gz -C /opt/sketch
RUN dnf install -y bison flex g++ java && dnf clean all
COPY hole_hardcoder.patch /opt/sketch/sketch-backend
RUN cd /opt/sketch/sketch-backend && patch -p0 --binary < hole_hardcoder.patch && ./configure && make -j

COPY staged_synth.opam /tmp
RUN opam install -y --deps-only /tmp/staged_synth.opam

RUN echo 'PATH=$PATH:/opt/sketch/sketch-frontend' >> /root/.bashrc && \
    echo 'SKETCH_HOME=/opt/sketch/sketch-frontend/runtime' >> /root/.bashrc && \
    echo 'eval $(opam env)' >> /root/.bashrc && \
    echo 'git config --global --add safe.directory /work/staged-synth' >> /root/.bashrc

CMD /bin/bash -i

# Local Variables:
# mode: dockerfile
# End:
