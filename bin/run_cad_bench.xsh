#!/usr/bin/env xonsh

import glob
import random

run_beam = True
run_exhaustive = True
run_local = True

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
            for r in range(25):
                jobs += [(f, c, r, gt)]

print('Jobs: ', len(jobs))
with open('jobs', 'w') as fl:
    for job_spec in jobs:
        fl.write(' '.join([str(x) for x in job_spec]) + '\n')

job_name = "metric-{1/}-{#}"
cmd = "%s/bin/metric_synth_cad.exe -max-cost {2} -verbosity 1 -group-threshold 0.4 -scaling 2 -n-groups {4} -dump-search-space %s.bin -out %s.json -backward-pass-repeats 5 -local-search-steps 500 < {1} 2> %s.log" % (build_dir, job_name, job_name, job_name)
print('Cmd: ', cmd)
parallel -j 20 --eta --joblog metric_joblog --timeout 10m --colsep ' ' --memsuspend 8G @(cmd) :::: jobs

if run_beam:
    job_name = "beam-{1/}-{#}"
    cmd = "%s/bin/metric_synth_cad.exe -max-cost {2} -verbosity 1 -group-threshold 0.0 -scaling 2 -n-groups {4} -dump-search-space %s.bin -out %s.json -backward-pass-repeats 5 -local-search-steps 500 < {1} 2> %s.log" % (build_dir, job_name, job_name, job_name)
    print('Cmd: ', cmd)
    parallel -j 20 --eta --joblog metric_joblog --timeout 10m --colsep ' ' --memsuspend 8G @(cmd) :::: jobs

if run_exhaustive:
    jobs = []
    for (d, c) in [('tiny', 10), ('small', 20), ('medium', 30), ('large', 40)]:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            jobs += [(f, c)]

    with open('exhaustive_jobs', 'w') as fl:
        for job_spec in jobs:
            fl.write(' '.join([str(x) for x in job_spec]) + '\n')

    job_name = "exhaustive-{1/}-{#}"
    cmd = "%s/bin/metric_synth_cad.exe -max-cost {2} -verbosity 1 -group-threshold 0.0 -scaling 2 -n-groups 100000000000 -dump-search-space %s.bin -out %s.json -backward-pass-repeats 0 -local-search-steps 0 < {1} 2> %s.log" % (build_dir, job_name, job_name, job_name)
    print('Cmd: ', cmd)
    parallel -j 20 --eta --joblog metric_joblog --timeout 10m --colsep ' ' --memsuspend 8G @(cmd) :::: jobs

if run_local:
    jobs = []
    for (d, c) in [('tiny', 10), ('small', 20), ('medium', 30), ('large', 40)]:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            for r in range(25):
                jobs += [(f, c, r)]

    with open('local_jobs', 'w') as fl:
        for job_spec in jobs:
            fl.write(' '.join([str(x) for x in job_spec]) + '\n')

    job_name = "local-{1/}-{#}"
    cmd = "%s/bin/metric_synth_cad.exe -max-cost {2} -verbosity 1 -group-threshold 1.0 -scaling 2 -dump-search-space %s.bin -out %s.json -backward-pass-repeats 5 -local-search-steps 500 < {1} 2> %s.log" % (build_dir, job_name, job_name, job_name)
    print('Cmd: ', cmd)
    parallel -j 20 --eta --joblog metric_joblog --timeout 10m --colsep ' ' --memsuspend 8G @(cmd) :::: jobs

# Local Variables:
# major-mode: python-mode
# End:
