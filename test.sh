#!/bin/bash


function ctrl_c() {
    echo "Caught C-C";
    parallel 'dot -Tpng {} > {.}.png' ::: *.dot;
    exit 1
}
trap ctrl_c INT

rm -f *dot *png;
dune exec bin/lazy_cegis_old.exe -- -num-inputs 4 -seed 8 -check -max-cost 6 -output-graph 8;
parallel 'dot -Tpng {} > {.}.png' ::: *.dot
