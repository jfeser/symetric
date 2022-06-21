#!/usr/bin/env xonsh

import glob
import json
import random

dry_run = False
run_metric = True

mlimit = 4 * 1000000 # 4GB
tlimit = 60          # 1min

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_regex.exe

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
ulimit_stanza = f"ulimit -v {mlimit}; ulimit -t {tlimit};"

if run_metric:
    for c in [10, 20, 30]:
        for t in [0.01, 0.1, 0.2]:
            for f in glob.glob(base_dir + '/vendor/regel/exp/so/benchmark/*'):
                # standard
                job_name = f"metric-{len(jobs)}"
                cmd = [
                    ulimit_stanza,
                    f"{build_dir}/bin/metric_synth_regex.exe -max-cost {c} -verbosity 2",
                    f"-group-threshold {t} -n-groups 200",
                    f"-out {job_name}.json -backward-pass-repeats 20",
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

parallel --will-cite --eta -j 15 --joblog joblog :::: jobs

# Local Variables:
# mode: python
# End:
