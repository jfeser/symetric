#!/usr/bin/env xonsh

import glob
import json
import os
import random

dry_run = False
run_enum = False
run_metric = True
run_extract_ablation = False
run_repair_ablation = False
run_rank_ablation = False
run_cluster_ablation = False

mlimit = 4 * 1000000 # 4GB
tlimit = 600

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_tower.exe bin/enumerate_tower.exe

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

def mk_cmd(max_cost, n_groups, group_threshold, job_name, extra_args, bench_file):
    return ' '.join([
        ulimit_stanza,
        f"{build_dir}/bin/metric_synth_tower.exe -max-cost {max_cost} -verbosity 2 -out {job_name}.json",
        f"-group-threshold {group_threshold} -n-groups {n_groups} -backward-pass-repeats 10 -local-search-steps 100",
        extra_args,
        f"< {bench_file} 2> {job_name}.log\n"
    ])


jobs = []
for f in glob.glob(base_dir + '/bench/tower/test*.sexp'):
    bench_name = os.path.basename(f)

    if run_enum:
        job_name = f"enum-tower-{len(jobs)}"
        cmd = ' '.join([
            f"ulimit -v {mlimit}; ulimit -c 0; timeout {tlimit}s",
            f"{build_dir}/bin/enumerate_tower.exe -out {job_name}.json",
            f"< {f} 2> {job_name}.log\n",
        ])
        jobs.append(cmd)

    for (c, g) in [(40, 100)]:
        for t in [0.4]:
            def mk_simple_cmd(job_name, extra_args=""):
                return mk_cmd(c, g, t, job_name, extra_args, f)

            # standard
            if run_metric:
                jobs.append(mk_simple_cmd(f"metric-tower-standard-{len(jobs)}"))

            # extract random
            if run_extract_ablation:
                jobs.append(mk_simple_cmd(f"metric-tower-extractrandom-{len(jobs)}", "-extract random"))

            # repair random
            if run_repair_ablation:
                jobs.append(mk_simple_cmd(f"metric-tower-repairrandom-{len(jobs)}", "-repair random"))

            # no rank
            if run_rank_ablation:
                jobs.append(mk_simple_cmd(f"metric-tower-norank-{len(jobs)}", "-use-ranking false"))

        if run_cluster_ablation:
            jobs.append(mk_cmd(c, g, 0, f"metric-tower-nocluster-{len(jobs)}", "", f))

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
