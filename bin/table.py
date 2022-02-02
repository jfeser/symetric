#!/usr/bin/env python3

import csv
import io
import json
import os
import sys

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
        "max_cost_generated",
        "space_contains_target",
    ]
    results = []
    for fn in os.listdir(run_dir):
        if not fn.endswith(".json"):
            continue
        with open(os.path.join(run_dir, fn), "r") as f:
            bench_json = json.load(f)

        method = bench_json["method"]
        max_cost = bench_json["params"]["max_cost"]
        group_count = bench_json["params"]["target_groups"]
        thresh = bench_json["params"]["group_threshold"]

        bench_params = os.path.splitext(fn)[0].split("-")
        bench_name = bench_params[-1]

        result_row = [
            method,
            bench_name,
            int(max_cost),
            int(group_count),
            float(thresh),
            bench_json["program"] is not None,
            runtime(bench_json),
            bench_json["program_size"],
            bench_json["stats"]["max_cost_generated"],
            bench_json["stats"]["space_contains_target"],
        ]
        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(["bench", "method"])
    return df


def process(df):
    print(df.to_csv())


def main(run_dir):
    process(load(run_dir))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: table.py RUN_DIR")
        exit(1)
    main(sys.argv[1])
