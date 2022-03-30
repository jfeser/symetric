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

plt.rcParams["text.usetex"] = True
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


def make_main_table(metric_df, beam_df, count, main_table):
    with open(main_table, "w") as f:
        print(
            r"\begin{tabular}{lrrrrllrrrrrr}",
            file=f,
        )
        print(r"\toprule", file=f)
        print(
            r"Name & \multicolumn{4}{c}{Program Size} & \multicolumn{2}{c}{Succ./Fail./T.out} & \multicolumn{2}{c}{Beam Width} & \multicolumn{4}{c}{Expected Runtime (s)} \\",
            file=f,
        )
        print(
            r"& {$B$} & {$B+$} & {$M_{200}$} & {$M_{400}$} & {$M_{200}$} & {$M_{400}$} & {$B$} & {$B+$} & {$B$} & {$B+$} & {$M_{200}$} & {$M_{400}$} \\",
            file=f,
        )
        print(r"\midrule", file=f)
        for bench in natural_sort(list(set(metric_df.index.get_level_values(0)))):
            dfb = metric_df.loc[bench]
            print(bench_name(bench), file=f, end="")

            print(
                r"& %s & %s & %s & %s"
                % (
                    maybe_int_to_tex(beam_df.loc[bench, 0]["program_size"]),
                    maybe_int_to_tex(beam_df.loc[bench, 500]["program_size"]),
                    maybe_float_to_tex(
                        metric_df.loc[bench, 200][("program_size", "mean", 0.2)]
                    ),
                    maybe_float_to_tex(
                        metric_df.loc[bench, 400][("program_size", "mean", 0.2)]
                    ),
                ),
                file=f,
                end="",
            )

            print(
                r"& %s & %s"
                % (
                    success_failure_timeout(
                        metric_df.loc[bench, 200][("success", "mean", 0.2)],
                        metric_df.loc[bench, 200][("timeout", "mean", 0.2)],
                        count.loc[bench, 200, "metric", 0.2],
                    ),
                    success_failure_timeout(
                        metric_df.loc[bench, 400][("success", "mean", 0.2)],
                        metric_df.loc[bench, 400][("timeout", "mean", 0.2)],
                        count.loc[bench, 400, "metric", 0.2],
                    ),
                ),
                file=f,
                end="",
            )

            if not math.isnan(beam_df.loc[bench, 0]["runtime_cum"]):
                print(r"& %d" % beam_df.loc[bench, 0]["n_groups"], file=f, end="")
            else:
                print(r"& %s" % bot, file=f, end="")

            if not math.isnan(beam_df.loc[bench, 500]["runtime_cum"]):
                print(r"& %d" % beam_df.loc[bench, 500]["n_groups"], file=f, end="")
            else:
                print(r"& %s" % bot, file=f, end="")

            print(
                (
                    runtimes_to_tex(
                        [
                            beam_df.loc[bench, 0]["runtime_cum"],
                            beam_df.loc[bench, 500]["runtime_cum"],
                            metric_df.loc[bench, 200, "metric"][
                                "expected_time", "", 0.2
                            ],
                            metric_df.loc[bench, 400, "metric"][
                                "expected_time", "", 0.2
                            ],
                        ]
                    )
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


def process_metric(df):
    df["runtime"] = df["runtime"].fillna(60 * 15)

    df = df[df["method"] == "metric"]
    gb = df.groupby(by=["bench", "n_groups", "method", "threshold"])
    count = gb.size()
    df = gb.agg(["mean"])
    df["expected_time"] = (1.0 / df["success"]) * df["runtime"]

    return df.unstack(level=-1), count


def first_true(s):
    for (i, v) in s.iteritems():
        if v:
            return i
    return s.index[-1]


def process_beam(df):
    df = df[df["method"] == "beam"]
    df = df.sort_values(["bench", "local_search_steps", "n_groups"])
    gb = df.groupby(["bench", "local_search_steps"])
    df["runtime_cum"] = gb["runtime"].cumsum()
    success_idx = gb["success"].agg(first_true)
    df = df.loc[success_idx]
    df = df.set_index(["bench", "local_search_steps"])
    df["any_success"] = gb["success"].any()
    df["runtime_cum"][~df["any_success"]] = float("nan")
    return df


def process(metric_df, beam_df, main_table, gen_plot):
    ipdb.set_trace()
    metric_df, count = process_metric(metric_df)
    beam_df = process_beam(beam_df)

    ipdb.set_trace()
    make_main_table(metric_df, beam_df, count, main_table)

    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)

    bx = list(
        sorted(beam_df.xs(0, level="local_search_steps")["runtime_cum"].fillna(1e20))
    )
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label="Beam")

    bx = list(
        sorted(beam_df.xs(500, level="local_search_steps")["runtime_cum"].fillna(1e20))
    )
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label="Beam+")

    bx = list(
        sorted(
            metric_df.xs(200, level="n_groups")["expected_time", "", 0.2].replace(
                [np.inf], 1e10
            )
        )
    )
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label="Metric ($\gamma = 200$)")

    bx = list(
        sorted(
            metric_df.xs(400, level="n_groups")["expected_time", "", 0.2].replace(
                [np.inf], 1e10
            )
        )
    )
    by = range(1, len(bx) + 1)
    ax.plot(bx, by, label="Metric ($\gamma = 400$)")

    n_bench = len(set(beam_df.index.get_level_values(0)))
    ax.set_xscale("log")
    ax.set_ylim([0, n_bench + 1])
    ax.set_xlim([1e1, 1e4])
    ax.set_ylabel("Benchmarks solved")
    ax.set_xlabel("Time (s)")
    plt.legend(loc="upper left")
    plt.savefig(gen_plot)


def main(args):
    process(
        load(args.metric_generated),
        load(args.beam_generated),
        args.main_table,
        args.generated_plot,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Produce result tables")
    parser.add_argument(
        "--metric-generated",
        metavar="METRIC_RUN_DIR",
        help="directory containing benchmark output",
    )
    parser.add_argument(
        "--beam-generated",
        metavar="BEAM_RUN_DIR",
        help="directory containing benchmark output",
    )
    parser.add_argument("--main-table")
    parser.add_argument("--generated-plot")
    args = parser.parse_args()

    main(args)
