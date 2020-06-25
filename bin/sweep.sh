#!/bin/bash

WORKDIR=/home/feser/ocaml-workspace/staged-synth/

dune build --profile=release bin/lazy_cegis.exe

../_build/default/staged-synth/bin/lazy_cegis.exe -print-header 0 > "$WORKDIR/results.csv"

rm jobs
# parallel \ 'echo {1} {2} {3} 4 1 8' \
#          :::: <(seq 1 25) ::: 4 ::: first random pareto \
#          >> "$WORKDIR/jobs"

parallel \ 'echo {1} {2} {3} 4 1 64' \
         :::: <(seq 1 25) ::: 8 16 32 ::: first random pareto \
         >> "$WORKDIR/jobs"

parallel \ 'echo {1} {2} first 4 0 64' \
         :::: <(seq 1 25) ::: 8 16 32 \
         >> "$WORKDIR/jobs"

# parallel \ 'echo {1} {2} {3} 5 1 64' \
#          :::: <(seq 1 20) ::: 8 16 32 64 128 ::: first random pareto \
#          >> "$WORKDIR/jobs"

# parallel \ 'echo {1} {2} first 5 0 64' \
#          :::: <(seq 1 10) ::: 8 16 32 64 128 \
#          >> "$WORKDIR/jobs"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 30 --colsep ' ' \
         '../_build/default/staged-synth/bin/lazy_cegis.exe -num-inputs {2} -seed {1} -refine {3} -max-cost {4} -abstraction {5} -check {6} 2> /dev/null' \
         :::: jobs \
    >> "$WORKDIR/results.csv"

# 16,4,10,4,1,11,22,3,5,3,16,11,0,0,first
