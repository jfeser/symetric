#!/usr/bin/env python3

import os
import sys


def main(sketch, bdir):
    os.system('mkdir -p %s' % bdir)
    for i in range(100):
        os.system('dune exec ../bin/bench.exe -- -min 10 -max 10 -no-identity -seed %d %s > %s/bench%d.sexp' % (i, sketch, bdir, i))
    os.system('dune exec ../bin/synth.exe -- %s > %s/synth.cpp' % (sketch, bdir))
    os.system('cp -f ../etc/sexp.cpp ../etc/sexp.hpp Makefile %s/' % bdir)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print('Usage: mk_bench.py SKETCH DIR')
    main(*sys.argv[1:])
