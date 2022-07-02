#!/usr/bin/env xonsh

import glob
import json
import os
import random

dry_run = False
run_metric = True
run_extract_ablation = True
run_repair_ablation = True
run_rank_ablation = True
run_cluster_ablation = True

mlimit = 4 * 1000000 # 4GB
tlimit = 600

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_tower.exe

run_dir = runs_dir + $(date '+%Y-%m-%d-%H:%M:%S').strip()
if not dry_run:
    mkdir -p @(run_dir)
    cd @(run_dir)

if not dry_run:
    with open('job_params', 'w') as f:
        json.dump({
            'mlimit': mlimit,
            'tlimit': tlimit,
            'commit': $(git rev-parse HEAD),
        }, f)

ulimit_stanza = f"ulimit -v {mlimit}; timeout {tlimit}s"

jobs = []
for (c, g) in [(40, 100)]:
    for t in [0.4]:
        for f in glob.glob(base_dir + '/bench/tower/test*.sexp'):
            bench_name = os.path.basename(f)

            def mk_cmd(extra_args):
                return ' '.join([
                    ulimit_stanza,
                    f"{build_dir}/bin/metric_synth_tower.exe -max-cost {c} -verbosity 2",
                    f"-group-threshold {t} -n-groups {g} -backward-pass-repeats 10 -local-search-steps 100",
                    extra_args,
                    f"< {f} 2> {job_name}.log\n"
                ])

            # standard
            if run_metric:
                job_name = f"metric-tower-standard-{len(jobs)}"
                cmd = mk_cmd(f"-out {job_name}.json")
                jobs.append(cmd)

            # extract random
            if run_extract_ablation:
                job_name = f"metric-tower-extractrandom-{len(jobs)}"
                cmd = mk_cmd(f"-out {job_name}.json -extract random")
                jobs.append(cmd)

            # repair random
            if run_repair_ablation:
                job_name = f"metric-tower-repairrandom-{len(jobs)}"
                cmd = mk_cmd(f"-out {job_name}.json -repair random")
                jobs.append(cmd)

            # no rank
            if run_rank_ablation:
                job_name = f"metric-tower-norank-{len(jobs)}"
                cmd = mk_cmd(f"-out {job_name}.json -use-ranking false")
                jobs.append(cmd)

            if run_cluster_ablation:
                job_name = f"metric-tower-nocluster-{len(jobs)}"
                cmd = mk_cmd(f"-out {job_name}.json -group-threshold 0.0")
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
