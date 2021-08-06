#!/usr/bin/env bash

WORKDIR=/home/feser/work/ocaml-workspace/staged-synth
BUILD_DIR="$WORKDIR/_build/default/"
EXE=bin/lazy_cegis.exe
FULL_EXE=$BUILD_DIR/$EXE

dune build --profile=release "$EXE"
rm -f results.csv jobs
find random_pat_size_9/ > $WORKDIR/jobs
parallel --jobs 2 --eta --joblog "$WORKDIR/joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-diverse-nn -max-cost 10 -w 2 -d 0.1 -r 0.1 -local bounded -print-json true -diversity false -embed-file encoder_best.pt.tar {1} 2> /dev/null" \
         :::: $WORKDIR/jobs \
    >> "$WORKDIR/results.csv"
