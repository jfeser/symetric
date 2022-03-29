#!/usr/bin/env xonsh

import glob
import random

base_dir = $HOME + "/work/ocaml-workspace/staged-synth"
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"

cd @(base_dir)
$PATH = $PATH + ['/opt/ispc/bin']
dune build --profile=release "bin/metric_synth_cad.exe"
dune build --profile=release "bin/enumerate_cad.exe"

run_dir = runs_dir + $(date '+%Y-%m-%d-%H:%M:%S').strip()
mkdir -p @(run_dir)
cd @(run_dir)

jobs = []
for (d, c) in [('tiny', 10), ('small', 20), ('medium', 30), ('large', 40)]:
    for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
        for gt in [100, 200]:
            for r in range(10):
                for t in [0.4]:
                    jobs += [(f, c, r, gt, t)]

print('Jobs: ', len(jobs))
with open('jobs', 'w') as fl:
    for (f, c, r, gt, t) in jobs:
        fl.write(f'{f} {c} {r} {gt} {t}\n')

job_name = "metric-{2}-{3}-{4}-{5}-{1/}"
cmd = "%s/bin/metric_synth_cad.exe -max-cost {2} -verbosity 1 -group-threshold {5} -scaling 2 -n-groups {4} -dump-search-space %s.bin -out %s.json < {1} 2> %s.log" % (build_dir, job_name, job_name, job_name)
print('Cmd: ', cmd)
parallel -j 20 --eta --joblog metric_joblog --timeout 10m --colsep ' ' --memsuspend 8G @(cmd) :::: jobs
