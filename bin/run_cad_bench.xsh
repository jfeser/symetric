#!/usr/bin/env xonsh

import glob
import json
import os
import random
import sys

dry_run = False
run_metric = True
run_ablations = True
run_exhaustive = True
run_abstract = True
run_llm = False

run_extract_ablation = True
run_repair_ablation = True
run_rank_ablation = True
run_cluster_ablation = True
run_distance_ablation = True

mlimit = 4 * 1000000 # 4GB
tlimit = 1     # 1hr

base_dir = $(pwd).strip()
build_dir = base_dir + "/../_build/default/symetric/"

run_dir = sys.argv[1]
if not dry_run:
    cd @(run_dir)

jobs = []
benchmarks = [('tiny', 10), ('small', 20), ('generated', 35)]

ulimit_stanza = f"ulimit -v {mlimit}; timeout {tlimit}"

if run_abstract:
    for (d, _) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            bench_name = $(basename @(f)).strip()
            job_name = f"abstract-{bench_name}-{len(jobs)}"
            cmd = [
                ulimit_stanza,
                f"symetric abs-cad -scaling 2 -no-repl < {f} &> {job_name}.log"
            ]
            cmd = ' '.join(cmd) + '\n'
            jobs.append(cmd)

if run_exhaustive:
    for (d, _) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            bench_name = $(basename @(f)).strip()
            job_name = f"exhaustive-{bench_name}-{len(jobs)}"
            cmd = [
                ulimit_stanza,
                f"symetric enumerate-cad -verbose -scaling 2 < {f} &> {job_name}.log"
            ]
            cmd = ' '.join(cmd) + '\n'
            jobs.append(cmd)

def build_metric_command(bench_file,
                         max_cost,
                         name="metric",
                         group_threshold=0.2,
                         scaling=2,
                         n_groups = 200,
                         backward_pass_repeats = 1,
                         local_search_steps = 500,
                         extract = "exhaustive",
                         exhaustive_width=16,
                         repair="guided",
                         use_ranking="true",
                         extra_args=""):
    job_name = f"{name}-{len(jobs)}"
    cmd = [
        ulimit_stanza,
        f"symetric metric-cad -max-cost {max_cost} -verbosity 1",
        f"-group-threshold {group_threshold} -scaling {scaling} -n-groups {n_groups}",
        f"-out {job_name}.json -backward-pass-repeats {backward_pass_repeats}",
        f"-local-search-steps {local_search_steps} -extract {extract}",
        f"-exhaustive-width {exhaustive_width} -repair {repair} -use-ranking {use_ranking}",
        f"{extra_args} < {bench_file} 2> {job_name}.log\n"
    ]
    return ' '.join(cmd)

if run_metric:
    for (d, max_cost) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            jobs.append(build_metric_command(f, max_cost))


def build_llm_command(bench_file, repeat, name="llm"):
    bench_name = os.path.basename(bench_file)
    job_name = f"{name}-{bench_name}-{repeat}"
    return f"{base_dir}/bin/run_cad_gpt.py < {bench_file} > {job_name}.out\n"


if run_llm:
    for (d, _) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            for i in range(10):
                jobs.append(build_llm_command(f, i))


if run_ablations:
    for (d, max_cost) in [('generated', 35)]:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            # extract random
            if run_extract_ablation:
                jobs.append(build_metric_command(f, max_cost, name="metric-extractrandom", extract="random"))

            # repair random
            if run_repair_ablation:
                jobs.append(build_metric_command(f, max_cost, name="metric-repairrandom", repair="random"))

            # no rank
            if run_rank_ablation:
                jobs.append(build_metric_command(f, max_cost, name="metric-norank", use_ranking="false"))

            # no cluster
            if run_cluster_ablation:
                jobs.append(build_metric_command(f, max_cost, name="metric-nocluster",
                                                 group_threshold=0.0, extra_args="-use-beam-search"))

            # simple distance
            if run_distance_ablation:
                jobs.append(build_metric_command(f, max_cost, name="metric-simpledist",
                                                 extra_args="-distance jaccard"))

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
