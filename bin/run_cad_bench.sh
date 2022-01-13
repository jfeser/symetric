#!/usr/bin/env bash

BASE_DIR=$HOME/work/ocaml-workspace/staged-synth
BUILD_DIR="$BASE_DIR/_build/default/"
RUNS_DIR="$BASE_DIR/runs"

cd $BASE_DIR
dune build --profile=release "bin/metric_synth_cad.exe"
dune build --profile=release "bin/enumerate_cad.exe"

# Create a new dated directory and move to it
RUN_DIR=$RUNS_DIR/$(date '+%Y-%m-%d-%H:%M:%S')
mkdir -p $RUN_DIR
cd $RUN_DIR

# Run metric synthesis
parallel --eta --joblog metric_joblog --timeout 600 --colsep ' ' \
         "$BUILD_DIR/bin/metric_synth_cad.exe -max-cost {1} -group-threshold {2} < {3} > metric-{1}-{2}-{3/}.json 2> metric-{1}-{2}-{3/}.log" ::: 8 16 24 32 36 ::: 0.1 0.2 0.3 ::: $BASE_DIR/bench/cad_ext/*

# Run enumerator
parallel --eta --joblog metric_joblog --timeout 600 --colsep ' ' \
         "$BUILD_DIR/bin/enumerate_cad.exe -max-cost {1} < {2} > enumerate-{1}-{2/}.json 2> enumerate-{1}-{2/}.log" ::: 8 16 24 ::: $BASE_DIR/bench/cad_ext/*

# Run sketch
parallel --eta --joblog metric_joblog --timeout 600 --colsep ' ' \
         "$BUILD_DIR/bin/enumerate_cad.exe -max-cost {1} < {2} > enumerate-{1}-{2/}.json 2> enumerate-{1}-{2/}.log" ::: 8 16 24 ::: $BASE_DIR/bench/cad_ext/*
