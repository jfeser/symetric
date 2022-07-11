#!/usr/bin/env python3

import argparse
import csv
import io
import json
import math
import os
import sys
import re
import ipdb

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

pd.set_option("mode.chained_assignment", "raise")

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
pd.set_option("display.max_rows", 1000)


def load_v1(run_dir):
    header = [
        "method",
        "bench",
        "max_cost",
        "n_groups",
        "threshold",
        "success",
        "runtime",
        "program_size",
        "max_cost_generated",
        "space_contains_target",
    ]
    results = []
    for fn in os.listdir(run_dir):
        if not fn.endswith(".json"):
            continue
        bench_params = os.path.splitext(fn)[0].split("-")
        with open(os.path.join(run_dir, fn), "r") as f:
            data = f.read()
            data = data.replace("}{", "}\n\n{")
            lines = data.split("\n\n")
            bench_json = None
            if lines:
                json_str = lines[-1]
                print(json_str)
                try:
                    bench_json = json.loads(json_str)
                except json.decoder.JSONDecodeError:
                    pass

        method = bench_params[0]
        max_cost = bench_params[1]
        group_count = bench_params[3]
        thresh = bench_params[4]
        bench_name = bench_params[-1]

        result_row = [method]

        if method == "enumerate":
            result_row += [bench_params[-1], int(bench_params[1]), float("nan")]
        elif method == "metric":
            result_row += [bench_name, int(max_cost), int(group_count), float(thresh)]

        if bench_json is None:
            result_row += [False, float("nan"), float("nan"), float("nan"), None]
        else:
            result_row += [
                bench_json["program"] is not None,
                bench_json["runtime"],
                bench_json["program_size"],
                bench_json["max_cost_generated"],
                bench_json["space_contains_target"],
            ]

        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(["bench", "method"])
    return df


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
    ]
    results = []
    for fn in os.listdir(run_dir):
        if not fn.endswith(".json"):
            continue
        with open(os.path.join(run_dir, fn), "r") as f:
            bench_json = json.load(f)

        method = fn.split("-")[0]
        max_cost = bench_json["params"]["max_cost"]
        group_count = bench_json["params"]["target_groups"]
        thresh = bench_json["params"]["group_threshold"]
        local_search_steps = bench_json["params"]["local_search_steps"]
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

        bench_params = os.path.splitext(fn)[0].split("-")
        bench_name = bench_of_out_file(jobs, fn)

        result_row = [
            method,
            bench_name,
            int(max_cost),
            int(group_count),
            float(thresh),
            bench_json["program"] is not None,
            runtime(bench_json),
            bench_json["program_size"],
            timeout(bench_json),
            local_search_steps,
            fn,
            bench_json["params"]["backward_pass_repeats"],
            extract,
            repair,
            rank,
        ]
        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(["bench", "method"])
    return df


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

    df = pd.read_csv(run_dir + "/" + name, header=0, sep="\t")
    df["method"] = df["Command"].apply(classify_method)
    df["status"] = df.apply(classify_status, axis=1)
    df["bench"] = df.apply(bench_from_command, axis=1)
    df["bench_kind"] = df["bench"].apply(classify_bench)
    df[fields] = df.apply(routine_times, axis=1)
    return df


def load_sketch(logs):
    dfs = []
    for log in logs.split(","):
        name = os.path.basename(log)
        dfs.append(load_joblog(os.path.dirname(log), name))
    return pd.concat(dfs)


def success_failure_timeout(success_frac, timeout_frac, total):
    success = int(success_frac * total)
    timeouts = int(timeout_frac * total)
    failures = int(total - success - timeouts)
    return f"{success}/{failures}/{timeouts}"


bot = r"$\bot$"


def bench_name(bench):
    return (
        (" ".join([s.capitalize() for s in bench.split("_")]))
        .removeprefix("Bench")
        .strip()
    )


def maybe_int_to_tex(x):
    if math.isnan(x):
        return bot
    return "%d" % int(x)


def maybe_float_to_tex(x):
    if math.isnan(x) or math.isinf(x):
        return bot
    return "%.1f" % x


def runtimes_to_tex(xs):
    minr = min([x for x in xs if not math.isnan(x)])
    out = ""
    for x in xs:
        out += "& "
        if x == minr:
            out += r"\bfseries "
        out += maybe_float_to_tex(x)
    return out


def atoi(text):
    return int(text) if text.isdigit() else text


def natural_keys(text):
    """
    alist.sort(key=natural_keys) sorts in human order
    http://nedbatchelder.com/blog/200712/human_sorting.html
    (See Toothy's implementation in the comments)
    """
    return [atoi(c) for c in re.split(r"(\d+)", text)]


def natural_sort(l):
    return sorted(l, key=natural_keys)


def print_row(df, total, repeats=1):
    def perc(x, r, t):
        return 100 * ((x / r) / t)

    row = ""

    if "success" in list(df["status"]):
        s = df[df["status"] == "success"]
        row += "& {:.1f} & {:.0f}\%".format(
            float(s["median"]), float(perc(s["count"], repeats, total))
        )
    else:
        row += r"& -- & 0\%"

    if "memory" in list(df["status"]):
        s = df[df["status"] == "memory"]
        row += "& {0:.0f}\%".format(float(perc(s["count"], repeats, total)))
    else:
        row += r"& 0\%"

    if "time" in list(df["status"]):
        s = df[df["status"] == "time"]
        row += "& {0:.0f}\%".format(float(perc(s["count"], repeats, total)))
    else:
        row += r"& 0\%"

    return row


def make_main_table(df, fn, total_generated=25, total_handwritten=15):
    total = total_generated + total_handwritten

    df = df[df["method"] != "ablation"].copy()
    df.loc[
        df["bench"] == "rings", "status"
    ] = "time"  # hack to deal with rings expected time > 1h
    ipdb.set_trace()
    df_kind = df.groupby(["method", "bench_kind", "status"])["JobRuntime"].agg(
        ["median", "count"]
    )
    df_total = df.groupby(["method", "status"])["JobRuntime"].agg(["median", "count"])

    with open(fn, "w") as f:
        print(
            r"""
\begin{tabular}{llrrrr}
\toprule
Algorithm & Benchmark & Median Runtime (s) & Success & Memory & Timeout \\
\midrule
""",
            file=f,
        )
        methods = [
            (r"\name", "metric", 5),
            (r"\textsc{FTA-Syngar}", "abstract", 1),
            (r"\textsc{FTA-Syngar} (NR)", "abstract_norepl", 1),
            (r"\textsc{FTA-Basic}", "enumerate", 1),
            (r"\textsc{Sketch}", "sketch", 1),
        ]
        for (texname, name, repeats) in methods:
            print(r"%s " % texname, file=f)
            print(
                r"& Generated %s \\"
                % print_row(
                    df_kind.loc[name, "Generated"].reset_index(),
                    total_generated,
                    repeats,
                ),
                file=f,
            )
            print(
                r"& Hand-written %s \\"
                % print_row(
                    df_kind.loc[name, "Hand-written"].reset_index(),
                    total_handwritten,
                    repeats,
                ),
                file=f,
            )
            print(
                r"& All %s \\"
                % print_row(df_total.loc[name].reset_index(), total, repeats),
                file=f,
            )
        print(r"\bottomrule", file=f)
        print(r"\end{tabular}", file=f)


def method_table(df, fn):
    df = df[df["method"] == "metric"]
    df = (
        df[df["status"] == "success"]
        .groupby(["bench_kind", "bench"])
        .agg(["mean"])
        .reset_index()
    )
    by_type = df.groupby(["bench_kind"]).agg(["median", "max"]).reset_index()
    total = df.agg(["median", "max"])
    with open(fn, "w") as f:
        print(
            r"""
\begin{tabular}{lrrrrrr}
\toprule
Benchmark & \multicolumn{2}{c}{\textsc{ConstructXFTA}} & \multicolumn{2}{c}{\textsc{Extract}} & \multicolumn{2}{c}{\textsc{Repair}} \\
& Median & Max & Median & Max & Median & Max \\
\midrule
""",
            file=f,
        )
        for k in ["Generated", "Hand-written"]:
            print(
                "{} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} \\\\".format(
                    k,
                    float(
                        by_type[by_type["bench_kind"] == k]["xfta_time"]["mean"][
                            "median"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["xfta_time"]["mean"]["max"]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["extract_time"]["mean"][
                            "median"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["extract_time"]["mean"][
                            "max"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["repair_time"]["mean"][
                            "median"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["repair_time"]["mean"][
                            "max"
                        ]
                    ),
                ),
                file=f,
            )

        print(
            "All & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} \\\\".format(
                float(total.loc["median"]["xfta_time"]),
                float(total.loc["max"]["xfta_time"]),
                float(total.loc["median"]["extract_time"]),
                float(total.loc["max"]["extract_time"]),
                float(total.loc["median"]["repair_time"]),
                float(total.loc["max"]["repair_time"]),
            ),
            file=f,
        )
        print(r"\bottomrule", file=f)
        print(r"\end{tabular}", file=f)


def construct_table(df, fn):
    df = df[df["method"] == "metric"]
    df = (
        df[df["status"] == "success"]
        .groupby(["bench_kind", "bench"])
        .agg(["mean"])
        .reset_index()
    )
    by_type = df.groupby(["bench_kind"]).agg(["median", "max"]).reset_index()
    total = df.agg(["median", "max"])

    with open(fn, "w") as f:
        print(
            r"""
\begin{tabular}{lrrrrrr}
\toprule
Benchmark & \multicolumn{2}{c}{Expansion} & \multicolumn{2}{c}{Clustering} & \multicolumn{2}{c}{Ranking} \\
& Median & Max & Median & Max & Median & Max \\
\midrule
""",
            file=f,
        )
        for k in ["Generated", "Hand-written"]:
            print(
                "{} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} \\\\".format(
                    k,
                    float(
                        by_type[by_type["bench_kind"] == k]["expansion_time"]["mean"][
                            "median"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["expansion_time"]["mean"][
                            "max"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["cluster_time"]["mean"][
                            "median"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["cluster_time"]["mean"][
                            "max"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["rank_time"]["mean"][
                            "median"
                        ]
                    ),
                    float(
                        by_type[by_type["bench_kind"] == k]["rank_time"]["mean"]["max"]
                    ),
                ),
                file=f,
            )

        print(
            "All & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} & {:.1f} \\\\".format(
                float(total.loc["median"]["expansion_time"]),
                float(total.loc["max"]["expansion_time"]),
                float(total.loc["median"]["cluster_time"]),
                float(total.loc["max"]["cluster_time"]),
                float(total.loc["median"]["rank_time"]),
                float(total.loc["max"]["rank_time"]),
            ),
            file=f,
        )
        print(r"\bottomrule", file=f)
        print(r"\end{tabular}", file=f)


def success_by_time(df, method, ngroups):
    return list(
        sorted(
            df.xs(ngroups, level="n_groups")[("expected_time", "", method)]
            .replace([np.inf], 1e20)
            .dropna()
        )
    )


def process_metric(df, timeout=3600):
    mask = df["bench"].str.startswith("bench_")
    df = df[mask].copy()

    timed_out = (df["runtime"] > timeout) | (df["runtime"].isna())
    df.loc[timed_out, "runtime"] = timeout
    df.loc[timed_out, "program_size"] = float("nan")
    df.loc[timed_out, "success"] = False

    gb = df.groupby(by=["method", "bench", "n_groups", "extract", "repair", "rank"])
    count = gb.size()
    df = gb.agg(["mean"])
    df["expected_time"] = (1.0 / df["success"]) * df["runtime"]

    return df, count


def first_true(s):
    for (i, v) in s.iteritems():
        if v:
            return i
    return s.index[-1]


def ablation_plot(metric_df, beam_df, er_df, gen_plot):
    metric_df, count = process_metric(metric_df)
    er_df, _ = process_metric(er_df)

    plt.tight_layout()
    fig = plt.figure(figsize=(9, 3.2))
    ax = fig.add_subplot(1, 1, 1)

    std_200 = (
        metric_df.xs(200, level="n_groups")
        .xs("Greedy", level="extract")
        .xs("Guided", level="repair")
        .xs(True, level="rank")["expected_time"]
    )
    std_200 = std_200.replace([np.inf], 1e10)
    bx = list(sorted(std_200))
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label=r"\textsc{SyMetric}", color="C2")

    no_group_200 = beam_df[
        (beam_df["n_groups"] == 200)
        & (beam_df["local_search_steps"] == 500)
        & (beam_df["method"] == "beam")
    ].copy()
    no_group_200.loc[~no_group_200["success"], "runtime"] = float("nan")
    bx = sorted(no_group_200["runtime"].fillna(1e20))

    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label=r"\textsc{NoCluster}", color="C0")

    # no_group_400 = beam_df[
    #     (beam_df["n_groups"] == 400)
    #     & (beam_df["local_search_steps"] == 500)
    #     & (beam_df["method"] == "beam")
    # ]
    # no_group_400["runtime"][~no_group_400["success"]] = float("nan")
    # bx = sorted(no_group_400["runtime"].fillna(1e20))
    # by = range(1, len(bx) + 1)
    # ax.plot(
    #     bx, by, label=r"\textsc{NoCluster} ($w = 400$)", linestyle="dotted", color="C0"
    # )

    # no_group_800 = beam_df[
    #     (beam_df["n_groups"] == 800)
    #     & (beam_df["local_search_steps"] == 500)
    #     & (beam_df["method"] == "beam")
    # ]
    # no_group_800["runtime"][~no_group_800["success"]] = float("nan")
    # bx = sorted(no_group_800["runtime"].fillna(1e20))
    # by = range(1, len(bx) + 1)
    # ax.plot(
    #     bx, by, label=r"\textsc{NoCluster} ($w = 800$)", linestyle="dashed", color="C0"
    # )

    # no_group_1600 = beam_df[
    #     (beam_df["n_groups"] == 1600)
    #     & (beam_df["local_search_steps"] == 500)
    #     & (beam_df["method"] == "beam")
    # ]
    # no_group_1600["runtime"][~no_group_1600["success"]] = float("nan")
    # bx = sorted(no_group_1600["runtime"].fillna(1e20))
    # by = range(1, len(bx) + 1)
    # ax.plot(
    #     bx,
    #     by,
    #     label=r"\textsc{NoCluster} ($w = 1600$)",
    #     linestyle="dashdot",
    #     color="C0",
    # )

    norank_200 = (
        metric_df.xs(200, level="n_groups")
        .xs("Greedy", level="extract")
        .xs("Guided", level="repair")
        .xs(False, level="rank")["expected_time"]
    )
    norank_200 = norank_200.replace([np.inf], 1e10)
    bx = list(sorted(norank_200))
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label=r"\textsc{NoRank}", color="C1")

    extractrandom_200 = (
        er_df.xs(200, level="n_groups")
        .xs("Random", level="extract")
        .xs("Guided", level="repair")
        .xs(True, level="rank")["expected_time"]
    )
    extractrandom_200 = extractrandom_200.replace([np.inf], 1e10)
    bx = list(sorted(extractrandom_200))
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label=r"\textsc{ExtractRandom}", color="C5")

    repairrandom_200 = (
        metric_df.xs(200, level="n_groups")
        .xs("Greedy", level="extract")
        .xs("Random", level="repair")
        .xs(True, level="rank")["expected_time"]
    )
    repairrandom_200 = repairrandom_200.replace([np.inf], 1e10)
    bx = list(sorted(repairrandom_200))
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label=r"\textsc{RepairRandom}", color="C4")

    n_bench = 25
    ax.set_ylim([0, n_bench + 1])
    ax.set_xlim([5, 1e4])
    ax.set_xscale("log")
    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    ax.set_title(r"Effect of Ablations on \textsc{SyMetric} Performance")
    plt.legend(loc="upper left")
    plt.savefig(gen_plot, bbox_inches="tight")


def main(args):
    # ablation_plot(
    #     load(args.ablation_metric),
    #     load(args.ablation_no_cluster),
    #     load(args.ablation_extract_random),
    #     args.generated_plot,
    # )

    joblog_df = load_joblog(args.bench)
    joblog_df = joblog_df[
        (joblog_df["method"] == "abstract_norepl")
        | (joblog_df["method"] == "abstract")
        | (joblog_df["method"] == "enumerate")
    ]

    metric_joblog_df = load_joblog(args.bench_metric)
    sketch_joblog_df = load_sketch(args.bench_sketch)

    joblog_df = joblog_df.append(sketch_joblog_df)
    joblog_df = joblog_df.append(metric_joblog_df)

    make_main_table(joblog_df, args.main_table)
    method_table(joblog_df, args.method_table)
    construct_table(joblog_df, args.construct_table)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Produce result tables")
    parser.add_argument(
        "--ablation-metric",
        metavar="METRIC_RUN_DIR",
        help="directory containing benchmark output",
    )
    parser.add_argument(
        "--ablation-extract-random",
        metavar="ER_RUN_DIR",
        help="directory containing benchmark output",
    )
    parser.add_argument(
        "--ablation-no-cluster",
        metavar="BEAM_RUN_DIR",
        help="directory containing benchmark output",
    )
    parser.add_argument(
        "--bench",
        metavar="RUN_DIR",
        help="benchmark output",
    )
    parser.add_argument("--bench-metric")
    parser.add_argument(
        "--bench-sketch",
        metavar="SKETCH_RUN_DIR",
        help="benchmark output",
    )
    parser.add_argument("--main-table")
    parser.add_argument("--method-table")
    parser.add_argument("--construct-table")
    parser.add_argument("--generated-plot")
    args = parser.parse_args()

    main(args)
