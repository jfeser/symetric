#!/usr/bin/env xonsh

import glob
import json
import random

dry_run = False
run_metric = False
run_beam = False
run_exhaustive = False
run_local = False
run_sketch = True

run_handwritten = False
run_generated = True

metric_mlimit = 2 * 1000000 # 2GB
metric_tlimit = 15 * 60     # 15min
beam_mlimit = metric_mlimit
beam_tlimit = 60 * 60       # 1hr
sketch_tlimit = beam_tlimit / 60
sketch_mlimit = 9 * 1000000 # 9GB

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

dune build --profile=release "bin/metric_synth_cad.exe"
dune build --profile=release "bin/cad_to_sketch.exe"

run_dir = runs_dir + $(date '+%Y-%m-%d-%H:%M:%S').strip()
mkdir -p @(run_dir)
cp cad.sk cad_header.sk @(run_dir)/
cd @(run_dir)

jobs = []
benchmarks = []
if run_handwritten:
    benchmarks += [('small', 20), ('medium', 30), ('large', 40)]
if run_generated:
    benchmarks += [('generated', 35)]

with open('job_params', 'w') as f:
    json.dump({
        'metric-memlimit': metric_mlimit,
        'metric-timelimit':metric_tlimit,
        'beam-memlimit':beam_mlimit,
        'beam-timelimit': beam_tlimit,
        'commit': $(git rev-parse HEAD),
    }, f)

if run_sketch:
    for (d, max_cost) in benchmarks:
        for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
            bench_name = $(basename @(f)).strip()
            job_name = f"sketch-{bench_name}-{len(jobs)}"
            sketch_name = f"{bench_name}.sk"
            cmd = [
                f"{build_dir}/bin/cad_to_sketch.exe -scaling 2 < {f} > {sketch_name};",
                f"ulimit -v {sketch_mlimit};",
                f"sketch --fe-timeout {sketch_tlimit} -V5 {sketch_name} &> {job_name}.log"
            ]
            cmd = ' '.join(cmd) + '\n'
            jobs.append(cmd)

if run_metric:
    for _ in range(25):
        for (d, max_cost) in benchmarks:
            for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                for n_groups in [200]:
                    for thresh in [0.2]:
                        for repeats in [5, 10, 20, 40]:
                            job_name = f"metric-{len(jobs)}"
                            cmd = [
                                f"ulimit -v {metric_mlimit}; ulimit -t {metric_tlimit};",
                                f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                                f"-group-threshold {thresh} -scaling 2 -n-groups {n_groups}",
                                f"-out {job_name}.json -backward-pass-repeats {repeats}",
                                f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                            ]
                            cmd = ' '.join(cmd)
                            jobs.append(cmd)

if run_beam:
    for n_groups in [100, 200, 400, 800, 1600, 3200]:
        for (d, max_cost) in benchmarks:
            for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                for local_search in [0, 500]:
                    job_name = f"beam-{len(jobs)}"
                    cmd = [
                        f"ulimit -v {beam_mlimit}; ulimit -t {beam_tlimit};",
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-n-groups {n_groups} -scaling 2 -use-beam-search -backward-pass-repeats 1",
                        f"-local-search-steps {local_search}",
                        f"-out {job_name}.json < {f} 2> {job_name}.log\n"
                    ]
                    cmd = ' '.join(cmd)
                    jobs.append(cmd)

print('Jobs: ', len(jobs))

if dry_run:
    print(jobs)
    exit(0)

with open('jobs', 'w') as f:
    f.writelines(jobs)
parallel --eta -j 46 --joblog joblog :::: jobs

# Local Variables:
# major-mode: python-mode
# End:
