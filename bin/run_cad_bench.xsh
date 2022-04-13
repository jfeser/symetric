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
run_group_ablation = True

mlimit = 4 * 1000000 # 4GB
sketch_mlimit = 8 * 1000000 # 8GB
tlimit = 60 * 60     # 1hr
sketch_tlimit = 60   # 1hr

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

if not dry_run:
    dune build --profile=release bin/metric_synth_cad.exe bin/pixels.exe bin/abs_synth_cad.exe bin/enumerate_cad.exe

run_dir = runs_dir + $(date '+%Y-%m-%d-%H:%M:%S').strip()
if not dry_run:
    mkdir -p @(run_dir)
    cp cad.sk cad_header.sk @(run_dir)/
    cd @(run_dir)

if not dry_run:
    with open('job_params', 'w') as f:
        json.dump({
            'mlimit': mlimit,
            'tlimit': tlimit,
            'sketch-tlimit': sketch_tlimit,
            'commit': $(git rev-parse HEAD),
        }, f)

jobs = []
benchmarks = [('tiny', 10), ('small', 20), ('generated', 35)]

ulimit_stanza = f"ulimit -v {mlimit}; ulimit -t {tlimit};"
sketch_ulimit_stanza = f"ulimit -v {sketch_mlimit}; ulimit -t {tlimit};"

if run_abstract:
    for (d, _) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            for repl in [True, False]:
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

sketch_jobs = []
if run_sketch:
    for (d, max_cost) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            if max_cost <= 10:
                height = 2
            elif max_cost <= 20:
                height = 4
            else:
                height = 6

            bench_name = $(basename @(f)).strip()
            job_name = f"sketch-{bench_name}-{len(jobs)}"
            sketch_name = f"{bench_name}.sk"
            cmd = [
                f"{build_dir}/bin/pixels.exe -scaling 2 < {f} > {bench_name}.in;",
                f"sed 's/INFILE/{bench_name}.in/' cad.sk > {sketch_name};",
                sketch_ulimit_stanza,
                f"sketch --bnd-inbits 10 --slv-nativeints -V5 --fe-output-test --bnd-unroll-amnt 5 --bnd-cbits 4 --bnd-int-range 3000 --fe-def DEPTH={height} --fe-def SCALING=2 {sketch_name} &> {job_name}.log"
            ]
            cmd = ' '.join(cmd) + '\n'
            sketch_jobs.append(cmd)

if run_metric:
    for _ in range(5):
        for (d, max_cost) in benchmarks:
            for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                for n_groups in [200, 400]:
                    # standard
                    job_name = f"metric-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups}",
                        f"-out {job_name}.json -backward-pass-repeats 20",
                        f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

if run_ablations:
    for _ in range(5):
        for (d, max_cost) in [('generated', 35)]:
            for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                for n_groups in [200, 400]:
                    # extract random
                    if run_extract_ablation:
                        job_name = f"metric-extractrandom-{len(jobs)}"
                        cmd = [
                            ulimit_stanza,
                            f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                            f"-group-threshold 0.2 -scaling 2 -n-groups {n_groups} -extract random",
                            f"-out {job_name}.json -backward-pass-repeats 20",
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
                            f"-out {job_name}.json -backward-pass-repeats 20",
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
                            f"-out {job_name}.json -backward-pass-repeats 20",
                            f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                        ]
                        cmd = ' '.join(cmd)
                        jobs.append(cmd)

    if run_group_ablation:
        for n_groups in [100, 200, 400, 800, 1600]:
            for (d, max_cost) in benchmarks:
                for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                    job_name = f"metric-nocluster-{len(jobs)}"
                    cmd = [
                        ulimit_stanza,
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-n-groups {n_groups} -scaling 2 -use-beam-search -backward-pass-repeats 1",
                        f"-local-search-steps 500 -group-threshold 0.0",
                        f"-out {job_name}.json < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

print('Jobs: ', len(jobs))

if dry_run:
    print(''.join(jobs + sketch_jobs))
    exit(0)

with open('jobs', 'w') as f:
    f.writelines(jobs)
if run_sketch:
    with open('sketch_jobs', 'w') as f:
        f.writelines(sketch_jobs)

parallel --will-cite --eta -j 44 --joblog joblog :::: jobs
if run_sketch:
    parallel --will-cite --eta -j 22 --joblog sketch_joblog :::: sketch_jobs

# Local Variables:
# mode: python
# End:
