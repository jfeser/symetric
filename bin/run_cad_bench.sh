#!/bin/bash

WORKDIR=/home/feser/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"

find bench/cad2/random_size_9 > $WORKDIR/jobs

rm results.csv
parallel --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-abs -max-cost 9 -csv {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$WORKDIR/results.csv"

parallel --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-concrete -max-cost 9 -csv {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$WORKDIR/results.csv"
