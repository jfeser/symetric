#!/bin/bash

timeout -s INT 10m dune exec bin/lazy_cegis.exe -- cad bench/cad/microscope7_13.sexp > microscope7_13.out 2> /dev/null &
timeout -s INT 10m dune exec bin/lazy_cegis.exe -- cad bench/cad/microscope7_9.sexp > microscope7_9.out 2> /dev/null &
timeout -s INT 10m dune exec bin/lazy_cegis.exe -- cad bench/cad/microscope2_1.sexp > microscope2_1.out 2> /dev/null &
timeout -s INT 10m dune exec bin/lazy_cegis.exe -- cad bench/cad/satellite0_3.sexp > satellite0_3.out 2> /dev/null &
timeout -s INT 10m dune exec bin/lazy_cegis.exe -- cad bench/cad/camera5_10.sexp > camera5_10.out 2> /dev/null &
timeout -s INT 10m dune exec bin/lazy_cegis.exe -- cad bench/cad/camera8_4.sexp > camera8_4.out 2> /dev/null &
