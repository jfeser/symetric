#!/usr/bin/env python3

import csv
import io
import json
import os
import sys

import pandas as pd

def load(run_dir):
    header = ['method', 'bench', 'max_cost', 'distance_threshold', 'success']
    results = []
    for fn in os.listdir(run_dir):
        if not fn.endswith('.json'):
            continue
        bench_params = os.path.splitext(fn)[0].split('-')
        with open(os.path.join(run_dir, fn), 'r') as f:
            bench_json = None
            for line in f:
                try:
                    bench_json = json.loads(line)
                except json.decoder.JSONDecodeError:
                    continue

        method = bench_params[0]
        result_row = [method]
        if method == 'enumerate':
            result_row += [bench_params[2], int(bench_params[1]), float('nan')]
        elif method == 'metric':
            result_row += [bench_params[3], int(bench_params[1]), float(bench_params[2])]
        if bench_json is None:
            result_row += [False]
        else:
            result_row += [bench_json['program'] is not None]
        results += [result_row]

    df = pd.DataFrame(results, columns=header)
    df = df.sort_values(['bench', 'method'])
    return df


def process(df):
    print(df.to_csv())

def main(run_dir):
    process(load(run_dir))
        
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Usage: table.py RUN_DIR')
        exit(1)
    main(sys.argv[1])
