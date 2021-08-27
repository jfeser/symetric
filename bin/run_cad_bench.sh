#!/usr/bin/env bash

WORKDIR=/home/feser/work/ocaml-workspace/staged-synth
BUILD_DIR="$WORKDIR/_build/default/"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"
rm -f jobs
find random_small_pat_size_8_leaf_only -type f > $WORKDIR/jobs

parallel --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-baseline -max-cost 8 -print-json true {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$WORKDIR/baseline_results"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-local -max-cost 8 -print-json true {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
         >> "$WORKDIR/local_results"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-local -max-cost 8 -print-json true -rules-file rules.sexp {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
         >> "$WORKDIR/local_results_rules"
