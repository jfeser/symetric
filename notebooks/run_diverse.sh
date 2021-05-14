#!/bin/bash

WORKDIR=/home/feser/work/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
OUT_FILE="$WORKDIR/notebooks/diverse_results.csv"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

find $WORKDIR/bench/cad2/random_repl_size_9/ > $WORKDIR/jobs

rm -f "$OUT_FILE"
parallel --eta --joblog "$WORKDIR/notebooks/diverse_joblog" --timeout 30 --colsep ' ' \
         "$FULL_EXE {3} -max-cost 9 -csv {1} 2> /dev/null" \
         :::: $WORKDIR/jobs ::: {0..29} ::: cad-sample-naive cad-sample-diverse  \
    >> "$OUT_FILE"
