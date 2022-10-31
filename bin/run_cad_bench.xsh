#!/usr/bin/env xonsh

import glob
import json
import random

dry_run = False
run_metric = False
run_ablations = True
run_exhaustive = False
run_sketch = False
run_abstract = False

run_extract_ablation = True
run_repair_ablation = False
run_rank_ablation = False
run_cluster_ablation = False

run_extract_random = True
run_extract_greedy = True
run_extract_centroid = True
run_extract_exhaustive = True

mlimit = 4 * 1000000 # 4GB
tlimit = 60 * 60     # 1hr

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_cad.exe bin/pixels.exe bin/abs_synth_cad.exe bin/enumerate_cad.exe

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
benchmarks = [('tiny', 10), ('small', 20), ('generated', 35)]

ulimit_stanza = f"ulimit -v {mlimit}; ulimit -t {tlimit};"

if run_abstract:
    for (d, _) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            bench_name = $(basename @(f)).strip()
            job_name = f"abstract-{bench_name}-{len(jobs)}"
            cmd = [
                ulimit_stanza,
                f"{build_dir}/bin/abs_synth_cad.exe -scaling 2 -no-repl < {f} &> {job_name}.log"
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
                f"{build_dir}/bin/enumerate_cad.exe -verbose -scaling 2 < {f} &> {job_name}.log"
            ]
            cmd = ' '.join(cmd) + '\n'
            jobs.append(cmd)

if run_sketch:
    f = base_dir + '/bench/cad_ext/small/skew'
    max_cost = 20
    par = False
    parallel_stanza = '--slv-p-cpus 2 --slv-parallel' if par else ''
    if max_cost <= 10:
        height = 2
    elif max_cost <= 20:
        height = 4
    else:
        height = 6

    bench_name = $(basename @(f)).strip()
    job_name = f"sketch-{bench_name}-{len(jobs)}"
    cmd = [
        f"{build_dir}/bin/pixels.exe -scaling 2 < {f} > {job_name}.in;",
        f"sed 's/INFILE/{job_name}.in/' cad.sk > {job_name}.sk;",
        f"./timeout -t {tlimit} -s {mlimit}",
        f"sketch -V5 --fe-output-test --fe-def SCALING=2 --fe-def DEPTH={height}",
        f"--bnd-inbits 10 --bnd-unroll-amnt 5 --bnd-cbits 4 --bnd-int-range 3000 --bnd-inline-amnt {height + 1}",
        f"--slv-nativeints",
        parallel_stanza,
        f"{job_name}.sk &> {job_name}.log"
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
                         extract = "greedy",
                         exhaustive_width=4,
                         repair="guided",
                         use_ranking="true",
                         extra_args=""):
    job_name = f"{name}-{len(jobs)}"
    cmd = [
        ulimit_stanza,
        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
        f"-group-threshold {group_threshold} -scaling {scaling} -n-groups {n_groups}",
        f"-out {job_name}.json -backward-pass-repeats {backward_pass_repeats}",
        f"-local-search-steps {local_search_steps} -extract {extract}",
        f"-exhaustive-width {exhaustive_width} -repair {repair} -use-ranking {use_ranking}"
        f"{extra_args} < {bench_file} 2> {job_name}.log\n"
    ]
    return ' '.join(cmd)

if run_metric:
    for (d, max_cost) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            jobs.append(build_metric_command(f, max_cost))

if run_ablations:
    for (d, max_cost) in [('generated', 35)]:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            # extract random
            if run_extract_ablation and run_extract_random:
                jobs.append(build_metric_command(f, max_cost, name="metric-extractrandom", extract="random"))

            # extract centroid
            if run_extract_ablation and run_extract_centroid:
                jobs.append(build_metric_command(f, max_cost, name="metric-extractcentroid", extract="centroid"))

            # extract local
            if run_extract_ablation and run_extract_greedy:
                jobs.append(build_metric_command(f, max_cost, name="metric-extractgreedy", extract="greedy"))

            # extract exhaustive
            if run_extract_ablation and run_extract_exhaustive:
                for width in [2, 4, 8, 16]:
                    jobs.append(build_metric_command(f, max_cost, name="metric-exhaustive",
                                                     extract="exhaustive", exhaustive_width=width))

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

print('Jobs: ', len(jobs))

if dry_run:
    print(''.join(jobs))
    exit(0)

with open('jobs', 'w') as f:
    f.writelines(jobs)

parallel --will-cite --eta -j 32 --joblog joblog :::: jobs

# Local Variables:
# mode: python
# End:
