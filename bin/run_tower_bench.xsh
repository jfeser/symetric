#!/usr/bin/env xonsh

import glob
import json
import os
import random
import sys

dry_run = False
run_enum = True
run_metric = True
run_llm = False

mlimit = 16 * 1000000 # 16GB
tlimit = 600 # 10 minutes

base_dir = $(pwd).strip()
build_dir = base_dir + "/../_build/default/symetric/"

run_dir = sys.argv[1]
if not dry_run:
    cd @(run_dir)

ulimit_stanza = f"ulimit -v {mlimit}; ulimit -c 0; timeout {tlimit}s"

def mk_cmd(job_name, bench_file, extra_args="", max_cost = 40, n_groups=100, group_threshold=0.4):
    return ' '.join([
        ulimit_stanza,
        f"symetric metric-tower -max-cost {max_cost} -verbosity 2 -out {job_name}.json",
        f"-group-threshold {group_threshold} -n-groups {n_groups} -backward-pass-repeats 1 -local-search-steps 100",
        extra_args,
        f"< {bench_file} 2> {job_name}.log\n"
    ])

def build_llm_command(bench_file, name="llm"):
    bench_name = os.path.basename(bench_file)
    job_name = f"{name}-{bench_name}"
    return f"{base_dir}/bin/run_tower_gpt.py < {bench_file} > {job_name}.out\n"

jobs = []
for f in glob.glob(base_dir + '/bench/tower/test*.sexp'):
    bench_name = os.path.basename(f)

    if run_enum:
        job_name = f"enum-tower-{len(jobs)}"
        cmd = ' '.join([
            ulimit_stanza,
            f"symetric enumerate-tower -out {job_name}.json",
            f"< {f} 2> {job_name}.log\n",
        ])
        jobs.append(cmd)

    # standard
    if run_metric:
        jobs.append(mk_cmd(f"metric-tower-standard-{len(jobs)}", f))

    # standard
    if run_llm:
        jobs.append(build_llm_command(f))


print('Jobs: ', len(jobs))

if dry_run:
    print(''.join(jobs))
    exit(0)

with open('tower_jobs', 'w') as f:
    f.writelines(jobs)

parallel --will-cite -j 20 --eta --joblog tower_joblog :::: tower_jobs

# Local Variables:
# mode: python
# End:
