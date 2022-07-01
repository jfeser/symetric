#!/usr/bin/env xonsh

import glob
import json
import os
import random

dry_run = False
run_metric = True

mlimit = 4 * 1000000 # 4GB
tlimit = 1200          # 5min

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_tower.exe

run_dir = runs_dir + $(date '+%Y-%m-%d-%H:%M:%S').strip()
if not dry_run:
    mkdir -p @(run_dir)
    cp cad.sk cad_header.sk bin/timeout @(run_dir)/
    cd @(run_dir)

if not dry_run:
    with open('job_params', 'w') as f:
        json.dump({
            'mlimit': mlimit,
            'tlimit': tlimit,
            'commit': $(git rev-parse HEAD),
        }, f)

jobs = []
ulimit_stanza = f"ulimit -v {mlimit}; timeout {tlimit}s;"

if run_metric:
    for (c, g) in [(40, 40), (40, 80), (40, 160), (40, 320)]:
        for t in [0.2]:
            for f in glob.glob(base_dir + '/bench/tower/test*.sexp'):
                bench_name = os.path.basename(f)
                # standard
                job_name = f"metric-tower-{len(jobs)}"
                cmd = [
                    ulimit_stanza,
                    f"{build_dir}/bin/metric_synth_tower.exe -max-cost {c} -verbosity 2",
                    f"-group-threshold {t} -n-groups {g}",
                    f"-out {job_name}.json -backward-pass-repeats 10",
                    f"-local-search-steps 100 < {f} 2> {job_name}.log\n"
                ]
                cmd = ' '.join(cmd)
                jobs.append(cmd)

print('Jobs: ', len(jobs))

if dry_run:
    print(''.join(jobs))
    exit(0)

with open('jobs', 'w') as f:
    f.writelines(jobs)

parallel --will-cite --eta --joblog joblog :::: jobs

# Local Variables:
# mode: python
# End:
