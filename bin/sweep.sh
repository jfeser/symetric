#!/bin/bash

WORKDIR=/home/feser/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
EXE=bin/lazy_cegis_old.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

$FULL_EXE -print-header 0 > "$WORKDIR/results.csv"

rm jobs
# parallel \ 'echo {1} {2} {3} 4 1 8' \
#          :::: <(seq 1 25) ::: 4 ::: first random pareto \
#          >> "$WORKDIR/jobs"

parallel \ 'echo {1} {2} 7 32' \
         :::: <(seq 1 200) ::: 4 \
         >> "$WORKDIR/jobs"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE -seed {1} -num-inputs {2} -max-cost {3} -check {4} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$WORKDIR/results.csv"

# 16,4,10,4,1,11,22,3,5,3,16,11,0,0,first
