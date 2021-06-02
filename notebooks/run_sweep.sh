#!/bin/bash

if [ -n "$(git status --porcelain)" ]; then 
    echo "Working dir not clean";
    exit 1
fi

NAME=sweep$(git rev-parse --short HEAD)
WORKDIR=/home/feser/work/ocaml-workspace/staged-synth/
BUILD_DIR="$WORKDIR/../_build/default/staged-synth/"
OUT_FILE="$WORKDIR/notebooks/${NAME}_results.json"
EXE=bin/lazy_cegis.exe

cd $WORKDIR || exit
dune build --profile=release "$EXE"

FULL_EXE=/tmp/$(basename $EXE)
cp -f $BUILD_DIR/$EXE "$FULL_EXE"

find $WORKDIR/bench/cad2/random_repl_size_11/ -name "*.sexp" | sort | head -n30 > $WORKDIR/jobs

rm -f "$OUT_FILE"

parallel --eta --joblog "$WORKDIR/notebooks/${NAME}_joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-sample-diverse -max-cost 11 -print-json true -d 0.1 -w 5 -p {2} -seed {1} -diversity {3} {4} 2> /dev/null" \
         ::: {0..99} ::: 50 ::: true false :::: $WORKDIR/jobs \
         >> "$OUT_FILE"

parallel --eta --joblog "$WORKDIR/notebooks/${NAME}_joblog" --timeout 600 --colsep ' ' \
         "$FULL_EXE cad-baseline -max-cost 11 -print-json true {4} 2> /dev/null" \
         :::: $WORKDIR/jobs \
         >> "$OUT_FILE"

rm -f $WORKDIR/jobs
