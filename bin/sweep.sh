#!/bin/bash

WORKDIR=$HOME/work/ocaml-workspace/staged-synth
BUILD_DIR="$WORKDIR/_build/default"
EXE=bin/search_test.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

rm -f $WORKDIR/jobs
touch $WORKDIR/jobs

parallel \ 'echo {1} {2} {3}' \
         ::: 0.15 0.2 0.25 ::: 10 22 ::: $(ls $WORKDIR/bench/cad_ext/) \
         >> "$WORKDIR/jobs"

parallel --eta --joblog "$WORKDIR/joblog" --colsep ' ' \
         "$FULL_EXE -thresh {1} -cost {2} < $WORKDIR/bench/cad_ext/{3} 2>&1 > {3}_{1}_{2}.out " \
         :::: $WORKDIR/jobs \
    >> "$WORKDIR/results.csv"

