#!/bin/bash

WORKDIR=/home/feser/ocaml-workspace/staged-synth/

dune build bin/lazy_cegis.exe

../_build/default/staged-synth/bin/lazy_cegis.exe -print-header 0 > "$WORKDIR/results.csv"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 60 \
         '../_build/default/staged-synth/bin/lazy_cegis.exe -num-inputs {2} -seed {1} -refine {3} -max-cost 10 64 2> /dev/null' \
         :::: <(seq 1 100) ::: 8 16 32 64 128 ::: first random pareto \
    >> "$WORKDIR/results.csv"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 60 \
         '../_build/default/staged-synth/bin/lazy_cegis.exe -num-inputs {2} -seed {1} -max-cost 10 -abstraction 0 64 2> /dev/null' \
         :::: <(seq 1 100) ::: 8 16 32 64 128 \
         >> "$WORKDIR/results.csv"

