#!/usr/bin/env xonsh

import glob

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
        for gt in [1000, 2000, 4000]:
            for r in range(5):
                jobs += [(f, c, r, gt)]
print('Jobs: ', len(jobs))
with open('jobs', 'w') as fl:
    for (f, c, r, gt) in jobs:
        fl.write(f'{f} {c} {r} {gt}\n')

cmd = build_dir + "/bin/metric_synth_cad.exe -max-cost {2} -verbosity 1 -group-threshold 0.1 -scaling 2 -n-groups {4} < {1} > metric-{2}-{3}-{4}-{1/}.json 2> metric-{2}-{3}-{4}-{1/}.log"
print('Cmd: ', cmd)
parallel --eta --joblog metric_joblog --timeout 1200 --colsep ' ' --memsuspend 8G @(cmd) :::: jobs
         
 
