#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
from scipy.stats import ttest_ind
from collections import defaultdict
import math
import ujson
import os.path
import os
import tempfile

(_, fn) = tempfile.mkstemp()
os.system('jq -c -s < \"%s\" > \"%s\"' % (sys.argv[1], fn))
df = pd.read_json(fn, orient='records')
os.remove(fn)
print(df.describe())

# df.loc[df['found-program'] == 0, 'runtime'] = float('nan')
# df.loc[df['found-program'] == 0, 'eval-calls'] = float('nan')
df['bench'] = df['bench'].apply(os.path.basename)

def plot_vs_baseline(stat, diverse=1, ylabel=None, title=None):
    x = []
    y = []
    c = []
    bench = sorted(df['bench'].unique())
    for k in bench:
        baseline = df[(df['bench'] == k) & (df['synth'] == 'baseline')][stat].item()

        data = df[(df['bench'] == k) & (df['synth'] == 'sampling-diverse') & (df['diversity'] == diverse)]
        for _, row in data.iterrows():
            x.append(baseline)
            y.append(row[stat])
            c.append('blue' if row['found-program'] > 0 else 'red')

    fig, ax = plt.subplots()
    ax.scatter(x, y, c=c, s = 20, marker='x')

    xlim = ax.get_xlim()
    ylim = ax.get_ylim()
    
    lims = [
        np.min([ax.get_xlim(), ax.get_ylim()]),  # min of both axes
        np.max([ax.get_xlim(), ax.get_ylim()]),  # max of both axes
    ]
    ax.plot(lims, lims, 'k-', alpha=0.75, zorder=0)

    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    ax.set_ylabel('Sampling')
    ax.set_xlabel('Baseline')
    ax.set_title(title)
    
plot_vs_baseline('runtime', diverse=1, title='Runtime (ms)')
plt.savefig('runtime.pdf')
plt.close()

plot_vs_baseline('eval-calls', diverse=1, title='Calls to eval')
plt.savefig('eval.pdf')
