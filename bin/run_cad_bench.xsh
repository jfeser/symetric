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
            for repl in [False]:
                bench_name = $(basename @(f)).strip()
                job_name = f"abstract-{bench_name}-{len(jobs)}"
                repl_flag = "" if repl else "-no-repl"
                cmd = [
                    ulimit_stanza,
                    f"{build_dir}/bin/abs_synth_cad.exe -scaling 2 {repl_flag} < {f} &> {job_name}.log"
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
    # for (d, max_cost) in benchmarks:
    #     # for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
    #     for f in [base_dir + '/bench/cad_ext/small/skew']:

            # for par in [False]:
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

if run_metric:
    for (d, max_cost) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            # standard
            job_name = f"metric-{len(jobs)}"
            cmd = [
                ulimit_stanza,
                f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                f"-group-threshold 0.2 -scaling 2 -n-groups 200",
                f"-out {job_name}.json -backward-pass-repeats 20",
                f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
            ]
            cmd = ' '.join(cmd)
            jobs.append(cmd)

if run_ablations:
    for (d, max_cost) in [('generated', 35)]:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            for n_groups in [200]:
                # extract random
                if run_extract_ablation and run_extract_random:
                    job_name = f"metric-extractrandom-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -extract random",
                        f"-out {job_name}.json -backward-pass-repeats 1",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

                # extract centroid
                if run_extract_ablation and run_extract_centroid:
                    job_name = f"metric-extractcentroid-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -extract centroid",
                        f"-out {job_name}.json -backward-pass-repeats 1",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

                # extract local
                if run_extract_ablation and run_extract_greedy:
                    job_name = f"metric-extractgreedy-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -extract greedy",
                        f"-out {job_name}.json -backward-pass-repeats 1",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

                # extract exhaustive
                if run_extract_ablation and run_extract_exhaustive:
                    job_name = f"metric-exhaustive-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -extract exhaustive",
                        f"-out {job_name}.json -backward-pass-repeats 1",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

                # repair random
                if run_repair_ablation:
                    job_name = f"metric-repairrandom-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -repair random",
                        f"-out {job_name}.json -backward-pass-repeats 1",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

                # no rank
                if run_rank_ablation:
                    job_name = f"metric-norank-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -use-ranking false",
                        f"-out {job_name}.json -backward-pass-repeats 1",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

                # no cluster
                if run_cluster_ablation:
                    job_name = f"metric-nocluster-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -group-threshold 0.0",
                        f"-out {job_name}.json -backward-pass-repeats 1 -use-beam-search",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

print('Jobs: ', len(jobs))

if dry_run:
    print(''.join(jobs))
    exit(0)

with open('jobs', 'w') as f:
    f.writelines(jobs)

parallel --will-cite --eta -j 40 --joblog joblog :::: jobs

# Local Variables:
# mode: python
# End:
