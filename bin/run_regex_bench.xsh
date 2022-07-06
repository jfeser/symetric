#!/usr/bin/env xonsh

import glob
import json
import os
import random

dry_run = False
run_abs = True
run_enum = False
run_metric = False
run_extract_ablation = False
run_repair_ablation = False
run_rank_ablation = False
run_cluster_ablation = False

mlimit = 4 * 1000000 # 4GB
tlimit = 300          # 5min

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_regex.exe bin/abs_synth_regex.exe bin/enumerate_regex.exe

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

def mk_cmd(max_cost, n_groups, group_threshold, sketch, job_name, extra_args, bench_file):
    return ' '.join([
        f"ulimit -v {mlimit} -c 0; timeout {tlimit}s",
        f"{build_dir}/bin/metric_synth_regex.exe -max-cost {max_cost} -verbosity 1",
        f"-group-threshold {group_threshold} -n-groups {n_groups}",
        f"-sketch '{sketch}'",
        f"-out {job_name}.json -backward-pass-repeats 10",
        extra_args,
        f"-local-search-steps 100 < {bench_file} 2> {job_name}.log\n"
    ])


for f in glob.glob(base_dir + '/vendor/regel/exp/so/benchmark/*'):
    bench_name = os.path.basename(f)
    sketch_file = f'{base_dir}/vendor/regel/exp/so/sketch/{bench_name}'
    with open(sketch_file, 'r') as sketch_f:
        sketches = sketch_f.readlines()
        sketches = [s.lstrip('0123456789').strip() for s in sketches]

    for sketch in sketches:
        if run_abs:
            job_name = f"abs-regex-{len(jobs)}"
            cmd = ' '.join([
                f"ulimit -v {mlimit} -c 0; timeout {tlimit}s",
                f"{build_dir}/bin/abs_synth_regex.exe -sketch '{sketch}' -out {job_name}.json",
                f"< {f} 2> {job_name}.log\n",
            ])
            jobs.append(cmd)

        if run_enum:
            job_name = f"enum-regex-{len(jobs)}"
            cmd = ' '.join([
                f"ulimit -v {mlimit}; ulimit -c 0; timeout {tlimit}s",
                f"{build_dir}/bin/enumerate_regex.exe -sketch '{sketch}' -out {job_name}.json",
                f"< {f} 2> {job_name}.log\n",
            ])
            jobs.append(cmd)

        for (c, g) in [(20, 200)]:
            for t in [0.3]:
                def mk_simple_cmd(job_name, extra_args=""):
                    return mk_cmd(c, g, t, sketch, job_name, extra_args, f)

                # standard
                if run_metric:
                    jobs.append(mk_simple_cmd(f"metric-regex-standard-{len(jobs)}"))

                # extract random
                if run_extract_ablation:
                    jobs.append(mk_simple_cmd(f"metric-regex-extractrandom-{len(jobs)}", "-extract random"))

                # repair random
                if run_repair_ablation:
                    jobs.append(mk_simple_cmd(f"metric-regex-repairrandom-{len(jobs)}", "-repair random"))

                # no rank
                if run_rank_ablation:
                    jobs.append(mk_simple_cmd(f"metric-regex-norank-{len(jobs)}", "-use-ranking false"))

            # no rank
            if run_cluster_ablation:
                jobs.append(mk_cmd(c, g, 0, sketch, f"metric-regex-nocluster-{len(jobs)}", "", f))

print('Jobs: ', len(jobs))

if dry_run:
    print('\n'.join(jobs))
    exit(0)

with open('jobs', 'w') as f:
    f.writelines(jobs)

parallel --will-cite --eta -j 60 --joblog joblog :::: jobs

# Local Variables:
# mode: python
# End:
