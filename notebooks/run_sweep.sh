#!/bin/bash

NAME=sweep
WORKDIR=/home/feser/work/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
OUT_FILE="$WORKDIR/notebooks/${NAME}_results.csv"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

find $WORKDIR/bench/cad2/random_repl_size_9/ -name "*.sexp" | sort | head -n50 > $WORKDIR/jobs

rm -f "$OUT_FILE"
parallel --eta --joblog "$WORKDIR/notebooks/${NAME}_joblog" --timeout 60 --colsep ' ' \
         "$FULL_EXE cad-simple -max-cost 9 -csv -d 1.0 -w {3} -p {2} -seed {1} {4} 2> /dev/null" \
         ::: {0..99} ::: 25 50 100 200 ::: {1..3} :::: $WORKDIR/jobs \
    >> "$OUT_FILE"
