#!/usr/bin/env python3

import argparse
import csv
import io
import json
import math
import os
import sys

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


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


def load(run_dir):
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

        bench_params = os.path.splitext(fn)[0].split("-")
        bench_name = bench_params[1]

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
        ]
        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(["bench", "method"])
    return df


def success_failure_timeout(success_frac, timeout_frac, total):
    success = int(success_frac * total)
    timeouts = int(timeout_frac * total)
    failures = int(total - success - timeouts)
    return f"{success}/{failures}/{timeouts}"


bot = r"$\bot$"


def bench_name(bench):
    return " ".join([s.capitalize() for s in bench.split("_")])


def make_main_table(df, count, main_table):
    with open(main_table, "w") as f:
        print(
            r"\begin{tabular}{llS[table-format=2]S[table-format=2.1]llS[table-format=3.1]S[table-format=4.1]}",
            file=f,
        )
        print(r"\toprule", file=f)
        print(
            r"Name & $\gamma$ & \multicolumn{2}{c}{Program Size} & \multicolumn{2}{c}{Success/Failure/Timeout} & \multicolumn{2}{c}{Expected Time} \\",
            file=f,
        )
        print(
            r"& & {Beam} & {Metric} & {Beam} & {Metric} & {Beam} & {Metric} \\", file=f
        )
        print(r"\midrule", file=f)
        for bench in sorted(list(set(df.index.get_level_values(0)))):
            dfb = df.loc[bench]
            is_first_row = True
            for (n_groups, row) in dfb.iterrows():
                if is_first_row:
                    print(bench_name(bench), file=f, end="")
                    is_first_row = False

                print(r"& %d" % n_groups, file=f, end="")

                beam_psize = row[("program_size", "mean", "beam")]
                if math.isnan(beam_psize):
                    beam_psize = r"{\textemdash}"
                else:
                    beam_psize = "%d" % int(beam_psize)

                print(
                    r"& %s & %.1f"
                    % (
                        beam_psize,
                        row[("program_size", "mean", "metric")],
                    ),
                    file=f,
                    end="",
                )
                print(
                    r"& %s & %s"
                    % (
                        success_failure_timeout(
                            row[("success", "mean", "beam")],
                            row[("timeout", "mean", "beam")],
                            count[bench, n_groups, "beam"],
                        ),
                        success_failure_timeout(
                            row[("success", "mean", "metric")],
                            row[("timeout", "mean", "metric")],
                            count[bench, n_groups, "metric"],
                        ),
                    ),
                    file=f,
                    end="",
                )

                beam_time = None
                if row[("success", "mean", "beam")] <= 0:
                    if row[("timeout", "mean", "beam")] < 1:
                        beam_time = bot
                    else:
                        beam_time = r"{\textemdash}"
                else:
                    beam_time = "%.1f" % row[("expected_time", "", "beam")]

                print(
                    r"& %s & %.1f"
                    % (
                        beam_time,
                        row[("expected_time", "", "metric")],
                    ),
                    file=f,
                    end="",
                )
                print(r"\\", file=f)
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


def process(df, main_table):
    df["runtime"] = df["runtime"].fillna(600)

    df = df[(df["method"] == "beam") | (df["method"] == "metric")]
    gb = df.groupby(by=["bench", "n_groups", "method", "threshold"])
    count = gb.size()
    df = gb.agg(["mean"])
    df["expected_time"] = (1.0 / df["success"]) * df["runtime"]

    import pdb

    pdb.set_trace()

    df = df.unstack(level=-1)

    make_main_table(df, count, main_table)

    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)
    bx = success_by_time(df, "beam", 100)
    by = range(1, len(bx) + 1)
    ax.plot(bx, by)

    bx = success_by_time(df, "beam", 200)
    by = range(1, len(bx) + 1)
    ax.plot(bx, by)

    bx = success_by_time(df, "metric", 100)
    by = range(1, len(bx) + 1)
    ax.plot(bx, by)

    bx = success_by_time(df, "metric", 200)
    by = range(1, len(bx) + 1)
    ax.plot(bx, by)

    n_bench = len(set(df.index.get_level_values(0)))
    ax.set_xscale("log")
    ax.set_ylim([0, n_bench + 1])
    ax.set_xlim([0, 5000])
    plt.show()


def main(args):
    process(load(args.run_dir), args.main_table)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Produce result tables")
    parser.add_argument(
        "run_dir", metavar="RUN_DIR", help="directory containing benchmark output"
    )
    parser.add_argument("--main-table")
    args = parser.parse_args()

    main(args)
