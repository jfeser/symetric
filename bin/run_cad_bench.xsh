#!/usr/bin/env xonsh

import glob
import random

dry_run = False
run_metric = True
run_beam = True
run_exhaustive = False
run_local = False

metric_mlimit = 2 * 1000000 # 2GB
metric_tlimit = 15 * 60     # 15min
beam_mlimit = metric_mlimit
beam_tlimit = 60 * 60       # 1hr

base_dir = $(pwd).strip()
build_dir = base_dir + "/_build/default/"
runs_dir = base_dir + "/runs/"
print(base_dir, build_dir, runs_dir)

dune build --profile=release "bin/metric_synth_cad.exe"

run_dir = runs_dir + $(date '+%Y-%m-%d-%H:%M:%S').strip()
mkdir -p @(run_dir)
cd @(run_dir)

jobs = []
benchmarks = [('generated', 35)]

if run_metric:
    for _ in range(25):
        for (d, max_cost) in benchmarks:
            for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                for n_groups in [200, 400, 800]:
                    for thresh in [0.2, 0.4]:
                        job_name = f"metric-{len(jobs)}"
                        cmd = [
                            f"ulimit -v {metric_mlimit}; ulimit -t {metric_tlimit};",
                            f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                            f"-group-threshold {thresh} -scaling 2 -n-groups {n_groups}",
                            f"-dump-search-space {job_name}.bin -out {job_name}.json -backward-pass-repeats 5",
                            f"-local-search-steps 500 < {f} 2> {job_name}.log\n"
                        ]
                        cmd = ' '.join(cmd)
                        jobs.append(cmd)

if run_beam:
    for n_groups in [100, 200, 400, 800, 1600, 3200, 6400, 12800]:
        for (d, max_cost) in benchmarks:
            for f in glob.glob(base_dir + '/bench/cad_ext/' + d + '/*'):
                for local_search in [0, 500]:
                    job_name = f"beam-{len(jobs)}"
                    cmd = [
                        f"ulimit -v {beam_mlimit}; ulimit -t {beam_tlimit};",
                        f"{build_dir}/bin/metric_synth_cad.exe -max-cost {max_cost} -verbosity 1",
                        f"-n-groups {n_groups} -scaling 2 -use-beam-search -backward-pass-repeats 1",
                        f"-local-search-steps {local_search} -dump-search-space {job_name}.bin",
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
parallel --eta --joblog joblog :::: jobs

# Local Variables:
# major-mode: python-mode
# End:
