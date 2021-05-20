#!/bin/bash

NAME=sweep
WORKDIR=/home/feser/work/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
OUT_FILE="$WORKDIR/notebooks/${NAME}_results.csv"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

find $WORKDIR/bench/cad2/random_repl_size_11/ -name "*.sexp" | sort | head -n10 > $WORKDIR/jobs

rm -f "$OUT_FILE"
parallel --eta --joblog "$WORKDIR/notebooks/${NAME}_joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-sample-diverse -max-cost 11 -print-csv true -d 0.0 -w 0 -p {2} -seed {1} -diversity {3} {4} 2> /dev/null" \
         ::: {0..99} ::: 1000 5000 10000 ::: true false :::: $WORKDIR/jobs \
    >> "$OUT_FILE"
