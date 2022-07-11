import pandas as pd
import os
import json
import re
from tqdm import tqdm
import matplotlib.pyplot as plt


def runtime(results):
    runtime_variant = results["stats"]["runtime"]
    if runtime_variant[0] == "Stopped":
        return runtime_variant[1]
    else:
        return None


def timeout(results):
    runtime_variant = results["stats"]["runtime"]
    return 1.0 if runtime_variant[0] == "Started" else 0.0


def bench_of_out_file(jobs, out_file):
    for cmd in jobs:
        if out_file in cmd:
            return cmd.split(" ")[-3].split("/")[-1]


def load(run_dir):
    with open(run_dir + "/jobs", "r") as f:
        jobs = f.readlines()

    header = [
        "method",
        "bench",
        "max_cost",
        "n_groups",
        "threshold",
        "success",
        "runtime",
        "program_size",
        "timeout",
        "local_search_steps",
        "filename",
        "backward_pass_repeats",
        "extract",
        "repair",
        "rank",
        "repair_time",
        "xfta_time",
        "cluster_time",
        "extract_time",
        "rank_time",
        "expansion_time",
    ]
    results = []
    for fn in tqdm(os.listdir(run_dir)):
        if not fn.endswith(".json"):
            continue
        with open(os.path.join(run_dir, fn), "r") as f:
            bench_json = json.load(f)

        method = fn.split("-")[0]
        if "params" in bench_json:
            max_cost = int(bench_json["params"]["max_cost"])
            group_count = int(bench_json["params"]["target_groups"])
            thresh = float(bench_json["params"]["group_threshold"])
            local_search_steps = bench_json["params"]["local_search_steps"]
            backward_pass_repeats = bench_json["params"]["backward_pass_repeats"]
            if "extract" in bench_json["params"]:
                extract = bench_json["params"]["extract"][0]
            else:
                extract = None
            if "repair" in bench_json["params"]:
                repair = bench_json["params"]["repair"][0]
            else:
                repair = None
            if "use_ranking" in bench_json["params"]:
                rank = bench_json["params"]["use_ranking"]
            else:
                rank = None
        else:
            max_cost = None
            group_count = None
            thresh = None
            local_search_steps = None
            extract = None
            repair = None
            rank = None
            backward_pass_repeats = None

        if "stats" in bench_json:
            repair_time = bench_json["stats"].get("repair_time")
            xfta_time = bench_json["stats"].get("xfta_time")
            cluster_time = bench_json["stats"].get("cluster_time")
            extract_time = bench_json["stats"].get("extract_time")
            rank_time = bench_json["stats"].get("rank_time")
            expansion_time = bench_json["stats"].get("expansion_time")
        else:
            repair_time = None
            xfta_time = None
            cluster_time = None
            extract_time = None
            rank_time = None
            expansion_time = None

        bench_params = os.path.splitext(fn)[0].split("-")
        bench_name = bench_of_out_file(jobs, fn)

        result_row = [
            method,
            bench_name,
            max_cost,
            group_count,
            thresh,
            bench_json["program"] is not None,
            runtime(bench_json),
            bench_json["program_size"],
            timeout(bench_json),
            local_search_steps,
            fn,
            backward_pass_repeats,
            extract,
            repair,
            rank,
            repair_time,
            xfta_time,
            cluster_time,
            extract_time,
            rank_time,
            expansion_time,
        ]
        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(["bench", "method"])
    return df


def plot_ablations(df):
    plt.tight_layout()
    fig = plt.figure(figsize=(9, 3.2))
    ax = fig.add_subplot(1, 1, 1)

    std = df[
        (df["extract"] == "Greedy")
        & (df["repair"] == "Guided")
        & (df["threshold"] > 0)
        & (df["rank"])
    ]["runtime"]
    std = std.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(std), range(0, len(std) + 1), label=r"\textsc{SyMetric}", color="C2"
    )

    nocluster = df[
        (df["extract"] == "Greedy")
        & (df["repair"] == "Guided")
        & (df["threshold"] == 0)
        & (df["rank"])
    ]["runtime"]
    nocluster = nocluster.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(nocluster),
        range(0, len(nocluster) + 1),
        label=r"\textsc{NoCluster}",
        color="C0",
    )

    norank = df[
        (df["extract"] == "Greedy")
        & (df["repair"] == "Guided")
        & (df["threshold"] > 0)
        & (~df["rank"])
    ]["runtime"]
    norank = norank.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(norank),
        range(0, len(norank) + 1),
        label=r"\textsc{NoRank}",
        color="C1",
    )

    extractrandom = df[
        (df["extract"] == "Random")
        & (df["repair"] == "Guided")
        & (df["threshold"] > 0)
        & (df["rank"])
    ]["runtime"]
    extractrandom = extractrandom.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(extractrandom),
        range(0, len(extractrandom) + 1),
        label=r"\textsc{ExtractRandom}",
        color="C5",
    )

    repairrandom = df[
        (df["extract"] == "Greedy")
        & (df["repair"] == "Random")
        & (df["threshold"] > 0)
        & (df["rank"])
    ]["runtime"]
    repairrandom = repairrandom.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(repairrandom),
        range(0, len(repairrandom) + 1),
        label=r"\textsc{RepairRandom}",
        color="C4",
    )

    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    ax.set_title(r"Effect of Ablations on \textsc{SyMetric} Performance")
    return ax


def method_table(df, **kwargs):
    df = df[df["success"]].agg(["median", "max"])
    print(
        r"""
\begin{tabular}{lrrrrrr}
\toprule
Benchmark & \multicolumn{2}{c}{\textsc{ConstructXFTA}} & \multicolumn{2}{c}{\textsc{Extract}} & \multicolumn{2}{c}{\textsc{Repair}} \\
& Median & Max & Median & Max & Median & Max \\
\midrule
""",
        **kwargs
    )
    print(
        "All & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} \\\\".format(
            float(df.loc["median"]["xfta_time"]),
            float(df.loc["max"]["xfta_time"]),
            float(df.loc["median"]["extract_time"]),
            float(df.loc["max"]["extract_time"]),
            float(df.loc["median"]["repair_time"]),
            float(df.loc["max"]["repair_time"]),
        ),
        **kwargs
    )
    print(r"\bottomrule", **kwargs)
    print(r"\end{tabular}", **kwargs)


def construct_table(df, **kwargs):
    df = df[df["success"]].agg(["median", "max"])
    print(
        r"""
\begin{tabular}{lrrrrrr}
\toprule
Benchmark & \multicolumn{2}{c}{Expansion} & \multicolumn{2}{c}{Clustering} & \multicolumn{2}{c}{Ranking} \\
& Median & Max & Median & Max & Median & Max \\
\midrule
""",
        **kwargs
    )
    print(
        "All & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} \\\\".format(
            float(df.loc["median"]["expansion_time"]),
            float(df.loc["max"]["expansion_time"]),
            float(df.loc["median"]["cluster_time"]),
            float(df.loc["max"]["cluster_time"]),
            float(df.loc["median"]["rank_time"]),
            float(df.loc["max"]["rank_time"]),
        ),
        **kwargs
    )
    print(r"\bottomrule", **kwargs)
    print(r"\end{tabular}", **kwargs)


def classify_method(x):
    if "metric_synth_cad" in x:
        if (
            "-extract random" in x
            or "-use-ranking false" in x
            or "-repair random" in x
            or "-use-beam-search" in x
            or "-n-groups 400" in x
        ):
            return "ablation"
        return "metric"
    if "abs_synth_cad" in x:
        if "-no-repl" in x:
            return "abstract_norepl"
        else:
            return "abstract"
    if "enumerate_cad" in x:
        return "enumerate"
    if "sketch" in x:
        if "--slv-parallel" in x:
            return "sketch_par"
        return "sketch"
    return float("nan")


def bench_from_command(x):
    cmd = x["Command"]
    if x["method"] == "sketch":
        m = re.compile(r"sketch-([a-z_0-9]+)-[0-9]+.sk").search(cmd)
        if m:
            return m.group(1)
        return float("nan")
    else:
        m = re.compile(r"bench/cad_ext/.+/([a-z_0-9]+)").search(cmd)
        if m:
            return m.group(1)
        return float("nan")


def classify_bench(x):
    if "bench" in x:
        return "Generated"
    return "Hand-written"


out_re = re.compile("metric-[0-9]+\.json")


def load_joblog(run_dir, name="joblog"):
    def classify_status(x):
        if x["method"] == "sketch":
            logfile = run_dir + "/" + x["Command"].split("&>")[-1].strip()
            with open(logfile, "r") as f:
                log = f.read()
            if "Error: Must be SPARSE!!!" in log:
                return "failure_par"
            if "[SKETCH] DONE" in log:
                return "success"
            if "TIMEOUT" in log:
                return "time"
            if "MEM_RSS" in log:
                return "memory"

        e = x["Exitval"]
        if e == 0:
            return "success"
        elif e == 1:
            return "failure"
        elif e == 134:
            return "memory"
        elif e == 137:
            return "time"

    fields = [
        "repair_time",
        "xfta_time",
        "cluster_time",
        "extract_time",
        "rank_time",
        "expansion_time",
    ]

    def routine_times(x):
        if x["method"] == "metric":
            out_file = out_re.search(x["Command"]).group(0)
            with open(run_dir + "/" + out_file, "r") as f:
                out = json.load(f)
            ret = []
            for f in fields:
                if f in out["stats"]:
                    ret.append(out["stats"][f])
                else:
                    ret.append(float("nan"))
            return pd.Series(ret)
        else:
            return pd.Series([float("nan")] * len(fields))

    df = pd.read_csv(os.path.abspath(run_dir + "/" + name), header=0, sep="\t")
    df["method"] = df["Command"].apply(classify_method)
    df["status"] = df.apply(classify_status, axis=1)
    df["bench"] = df.apply(bench_from_command, axis=1)
    df["bench_kind"] = df["bench"].apply(classify_bench)
    df[fields] = df.apply(routine_times, axis=1)
    return df
