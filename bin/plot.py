import glob
import os
import json
import re
import sys

from tqdm import tqdm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


use_latex = False

n_cad_bench = 40
n_regex_bench = 2174
n_tower_bench = 34
tower_baseline_perc = 0.575
tower_baseline_solved = 40 * tower_baseline_perc

regel_label = r"\textsc{Regel}" if use_latex else "Regel"
fta_label = r"\textsc{FTA}" if use_latex else "FTA"
afta_label = r"\textsc{AFTA}" if use_latex else "AFTA"
symetric_label = r"\textsc{SyMetric}" if use_latex else "SyMetric"
sketch_label = r"\textsc{Sketch-Du}" if use_latex else "Sketch-Du"

if use_latex:
    plt.rcParams.update(
        {
            "font.family": "serif",
            "text.usetex": True,
            "pgf.rcfonts": False,
            "pgf.preamble": "\n".join(
                [r"\documentclass[acmsmall,review,timestamp]{acmart}"]
            ),
        }
    )


def runtime(results):
    if "synth" in results:
        runtime_variant = results["synth"]["stats"]["runtime"]
    else:
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
    jobs = []
    for fn in glob.glob(run_dir + "/*_jobs"):
        with open(fn, "r") as f:
            jobs += f.readlines()

    results = []
    for fn in tqdm(os.listdir(run_dir)):
        if not fn.endswith(".json"):
            continue
        with open(os.path.join(run_dir, fn), "r") as f:
            bench_json = json.load(f)

        method = "-".join(fn.split("-")[:-1])
        if "program" in bench_json:
            program = bench_json["program"]
        elif "synth" in bench_json:
            program = bench_json["synth"]["program"]
        else:
            raise Exception("No program in json")

        # max_cost = (
        #     int(bench_json["params"]["max_cost"])
        #     if "max_cost" in bench_json["params"]
        #     else None
        # )

        # if "params" in bench_json:
        #     max_cost = int(bench_json["params"]["max_cost"])
        #     group_count = int(bench_json["params"]["target_groups"])
        #     thresh = float(bench_json["params"]["group_threshold"])
        #     local_search_steps = bench_json["params"]["local_search_steps"]
        #     backward_pass_repeats = bench_json["params"]["backward_pass_repeats"]
        #     if "exhaustive_width" in bench_json["params"]:
        #         exhaustive_width = bench_json["params"]["exhaustive_width"]
        #     else:
        #         exhaustive_width = None
        #     if "extract" in bench_json["params"]:
        #         extract = bench_json["params"]["extract"][0]
        #     else:
        #         extract = None
        #     if "repair" in bench_json["params"]:
        #         repair = bench_json["params"]["repair"][0]
        #     else:
        #         repair = None
        #     if "use_ranking" in bench_json["params"]:
        #         rank = bench_json["params"]["use_ranking"]
        #     else:
        #         rank = None
        # else:
        #     max_cost = None
        #     group_count = None
        #     thresh = None
        #     local_search_steps = None
        #     extract = None
        #     repair = None
        #     rank = None
        #     backward_pass_repeats = None
        #     exhaustive_width = None

        # if "stats" in bench_json:
        #     repair_time = bench_json["stats"].get("repair_time")
        #     xfta_time = bench_json["stats"].get("xfta_time")
        #     cluster_time = bench_json["stats"].get("cluster_time")
        #     extract_time = bench_json["stats"].get("extract_time")
        #     rank_time = bench_json["stats"].get("rank_time")
        #     expansion_time = bench_json["stats"].get("expansion_time")
        # else:
        #     repair_time = None
        #     xfta_time = None
        #     cluster_time = None
        #     extract_time = None
        #     rank_time = None
        #     expansion_time = None

        bench_params = os.path.splitext(fn)[0].split("-")
        bench_name = bench_of_out_file(jobs, fn)

        header = [
            "method",
            "bench",
            "success",
            "runtime",
            # "program_size",
            # "timeout",
            # "local_search_steps",
            # "filename",
            # "backward_pass_repeats",
            # "extract",
            # "repair",
            # "rank",
            # "repair_time",
            # "xfta_time",
            # "cluster_time",
            # "extract_time",
            # "rank_time",
            # "expansion_time",
            # "exhaustive_width",
        ]

        result_row = [
            method,
            bench_name,
            program is not None,
            runtime(bench_json),
            # bench_json["program_size"],
            # timeout(bench_json),
            # local_search_steps,
            # fn,
            # backward_pass_repeats,
            # extract,
            # repair,
            # rank,
            # repair_time,
            # xfta_time,
            # cluster_time,
            # extract_time,
            # rank_time,
            # expansion_time,
            # exhaustive_width,
        ]
        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(["bench", "method"])
    return df


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
        **kwargs,
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
        **kwargs,
    )
    print(r"\bottomrule", **kwargs)
    print(r"\end{tabular}", **kwargs)


def classify_method(x):
    if "metric-cad" in x:
        if re.search("metric-[0-9]+.json", x) is not None:
            return "metric"
        else:
            return "ablation"
    if "abs-cad" in x:
        if "-no-repl" in x:
            return "abstract_norepl"
        else:
            return "abstract"
    if "enumerate-cad" in x:
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


out_re = re.compile("[a-z-]+-[0-9]+\.json")


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

        if x["method"] == "metric":
            out_file = out_re.search(x["Command"]).group(0)
            with open(run_dir + "/" + out_file, "r") as f:
                out = json.load(f)
                return "failure" if out["synth"]["program"] is None else "success"

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
                if f in out["synth"]["stats"]:
                    ret.append(out["synth"]["stats"][f])
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


def plot_regex(df, reg, filename="regex.pdf"):
    plt.tight_layout()
    fig = plt.figure(figsize=(5, 2.5))
    ax = fig.add_subplot(1, 1, 1)

    std = df[df["method"] == "metric-regex-standard"]["runtime"]
    std = std.sort_values().fillna(1e10)
    ax.plot([0] + list(std), range(0, len(std) + 1), label=symetric_label, color="C2")

    en = df[df["method"] == "enum-regex"]["runtime"]
    en = en.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(en),
        range(0, len(en) + 1),
        label=fta_label,
        color="C0",
    )

    abstr = df[df["method"] == "abs-regex"]["runtime"]
    abstr = abstr.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(abstr),
        range(0, len(abstr) + 1),
        label=afta_label,
        color="C1",
    )

    reg = reg["runtime"].sort_values().fillna(1e10)
    ax.plot(
        [0] + list(reg) + [1e10],
        range(0, len(reg) + 2),
        label=regel_label,
        color="C3",
    )

    ax.set_ylim([0, n_regex_bench])
    ax.set_xlim([1e-3, 400])
    ax.set_xscale("log")
    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    plt.legend(loc="upper left")
    ax.set_title(r"(c) Synthesis Performance on Regular Expressions")
    fig.savefig(filename, bbox_inches="tight")


def plot_regex_detail(df, reg, filename="regex-detail.pdf"):
    plt.tight_layout()
    fig = plt.figure(figsize=(5, 2.5))
    ax = fig.add_subplot(1, 1, 1)

    std = df[df["method"] == "metric-regex-standard"]["runtime"]
    std = std.sort_values().fillna(1e10)
    ax.plot([0] + list(std), range(0, len(std) + 1), label=symetric_label, color="C2")

    en = df[df["method"] == "enum-regex"]["runtime"]
    en = en.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(en),
        range(0, len(en) + 1),
        label=fta_label,
        color="C0",
    )

    reg = reg["runtime"].sort_values().fillna(1e10)
    ax.plot(
        [0] + list(reg) + [1e10],
        range(0, len(reg) + 2),
        label=regel_label,
        color="C3",
    )

    ax.set_ylim([600, 2000])
    ax.set_xlim([1, 400])
    ax.set_xscale("log")
    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    plt.legend(loc="lower right")
    ax.set_title(
        r"(d) Synthesis Performance on Regular Expressions (Detail)", fontsize=10
    )
    plt.savefig(filename, bbox_inches="tight")


def plot_tower(df, filename="tower.pdf"):
    plt.tight_layout()
    fig = plt.figure(figsize=(5, 2.5))
    ax = fig.add_subplot(1, 1, 1)

    std = df[(df["method"] == "metric-tower-standard")]["runtime"]
    std = std.sort_values().fillna(1e10)
    ax.plot([0] + list(std), range(0, len(std) + 1), label=symetric_label, color="C2")

    enum = df[(df["method"] == "enum-tower")]["runtime"]
    enum = enum.sort_values().fillna(1e10)
    ax.plot([0] + list(enum), range(0, len(enum) + 1), label=fta_label, color="C0")

    ax.axhline(y=tower_baseline_solved, linestyle="--", label="Neural", color="C5")

    ax.set_ylim([0, n_tower_bench])
    ax.set_xlim([1, 800])
    ax.set_xscale("log")
    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    ax.legend(loc="upper left", ncol=3)
    ax.set_title(r"(b) Synthesis Performance on Tower Building")
    plt.savefig(filename, bbox_inches="tight")


def plot_csg(df, filename="csg.pdf"):
    plt.tight_layout()
    fig = plt.figure(figsize=(5, 2.5))
    ax = fig.add_subplot(1, 1, 1)

    df.loc[df["status"] != "success", "JobRuntime"] = float("nan")
    df = df.groupby(["method", "bench"])["JobRuntime"].max()
    std = df.xs("metric")
    std = std.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(std) + [1e10],
        range(0, len(std) + 2),
        label=symetric_label,
        color="C2",
    )

    sketch = df.xs("sketch")
    sketch = sketch.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(sketch) + [1e10],
        range(0, len(sketch) + 2),
        label=sketch_label,
        color="C3",
    )

    absn = df.xs("abstract_norepl")
    absn = absn.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(absn) + [1e10], range(0, len(absn) + 2), label=afta_label, color="C1"
    )

    enum = df.xs("enumerate")
    enum = enum.sort_values().fillna(1e10)
    ax.plot(
        [0] + list(enum) + [1e10], range(0, len(enum) + 2), label=fta_label, color="C0"
    )

    ax.set_ylim([0, n_cad_bench])
    ax.set_xlim([1e0, 1e4])
    ax.set_xscale("log")
    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    plt.legend(loc="upper left")
    ax.set_title(r"(a) Synthesis Performance on Inverse CSG")
    plt.savefig(filename, bbox_inches="tight")


def plot_method(df, ax, dff, label_str):
    dff = dff.sort_values().fillna(1e10)
    label = f"\\textsc{{{label_str}}}" if use_latex else label_str
    ax.plot([0] + list(dff), range(0, len(dff) + 1), label=label)


def plot_csg_ablation(df, filename="csg-ablation.pdf"):
    plt.tight_layout()
    fig = plt.figure(figsize=(7, 2.5))
    ax = fig.add_subplot(1, 1, 1)
    df = df[
        df["method"].isin(
            [
                "metric",
                "metric-extractrandom",
                "metric-repairrandom",
                "metric-norank",
                "metric-nocluster",
                "metric-simpledist",
            ]
        )
    ]
    # TODO: ensure we only plot generated benchmarks
    # df = df[df["bench"].str.startswith("bench")]
    df.loc[~df["success"], "runtime"] = 1e10

    plot_method(df, ax, df[df["method"] == "metric"]["runtime"], "SyMetric")
    plot_method(
        df, ax, df[df["method"] == "metric-extractrandom"]["runtime"], "Extract random"
    )
    plot_method(
        df, ax, df[df["method"] == "metric-repairrandom"]["runtime"], "Repair random"
    )
    plot_method(df, ax, df[df["method"] == "metric-norank"]["runtime"], "No Rank")
    plot_method(df, ax, df[df["method"] == "metric-nocluster"]["runtime"], "No Cluster")
    plot_method(
        df, ax, df[df["method"] == "metric-simpledist"]["runtime"], "Simple distance"
    )

    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    ax.set_title(f"Effect of Ablations on {symetric_label} Performance")
    ax.set_xlim(5, 3600)
    ax.set_xscale("log")
    ax.legend(loc="upper left", ncol=3)
    plt.savefig(filename, bbox_inches="tight")

    return ax


def load_regel(d):
    times = []
    for fn in os.listdir(d):
        with open(d + "/" + fn, "r") as f:
            x = f.readlines()
        for line in x:
            if "Total time:" in line:
                time = float(line.split(":")[1])
                times.append(time)
                break
    return pd.DataFrame({"runtime": times})


if __name__ == "__main__":
    if len(sys.argv) < 2:
        run_dir = "runs/latest"
    else:
        run_dir = sys.argv[1]
    if not os.path.isdir(run_dir):
        print("Usage: plot.py <run_dir>")
        print("<run_dir> does not exist")
        exit(1)

    df = load(run_dir)

    regel_df = (
        load_regel(run_dir + "/regel")
        if os.path.isdir(run_dir + "/regel")
        else pd.DataFrame({"runtime": []})
    )

    cad_joblog_df = load_joblog(run_dir, "cad_joblog")
    cad_sketch_joblog_df = load_joblog(run_dir, "cad_sketch_joblog")
    cad_joblog_df = pd.concat([cad_joblog_df, cad_sketch_joblog_df])

    plot_csg(cad_joblog_df)
    plot_csg_ablation(df)
    plot_regex(df, regel_df)
    plot_regex_detail(df, regel_df)
    plot_tower(df)
