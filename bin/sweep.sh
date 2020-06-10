#!/bin/bash

WORKDIR=/home/feser/ocaml-workspace/staged-synth/

dune build bin/lazy_cegis.exe

../_build/default/staged-synth/bin/lazy_cegis.exe -print-header 0 > "$WORKDIR/results.csv"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 60 \
    '../_build/default/staged-synth/bin/lazy_cegis.exe -num-inputs {1} -seed {2} -abstraction {4} {3} 2> /dev/null' \
    :::: <(seq 1 4) :::: <(seq 1 100) :::: <(seq 2 10) ::: 0 1 \
    >> "$WORKDIR/results.csv"
