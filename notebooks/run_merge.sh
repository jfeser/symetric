#!/bin/bash

WORKDIR=/home/feser/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
OUT_FILE="$WORKDIR/notebooks/merge_results.csv"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

find bench/cad2/random_size_9_nprim_10/ > $WORKDIR/jobs
find bench/cad2/random_size_9_nprim_20/ >> $WORKDIR/jobs

rm -f "$OUT_FILE"
parallel --eta --joblog "$WORKDIR/notebooks/merge_joblog_abs" --timeout 30 --colsep ' ' \
         "$FULL_EXE cad-abs -max-cost 9 -csv {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$OUT_FILE"

parallel --eta --joblog "$WORKDIR/notebooks/merge_joblog_conc" --timeout 30 --colsep ' ' \
         "$FULL_EXE cad-concrete -max-cost 9 -csv {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$OUT_FILE"
