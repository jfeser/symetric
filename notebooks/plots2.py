#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
from scipy.stats import ttest_ind
from collections import defaultdict
import math
import ujson

rows = ujson.load(open(sys.argv[1], 'r'))

div = defaultdict(list)
ndiv = defaultdict(list)
for row in rows:
    if row['diversity']:
        out = div
    else:
        out = ndiv
    for i, dist in enumerate(row['value-dist']):
        if len(dist) >= row['per-cost']:
            out[i].append(pd.Series(dist).min())

dists = []
for k in sorted(div.keys()):
    dists.append(div[k])
    dists.append(ndiv[k])
plt.title('Diversity sampling (p=%d)' % rows[0]['per-cost'])
plt.ylabel('Min. Jaccard dist. to output')
plt.xlabel('Cost')
plt.xticks(np.arange(1.5, len(dists) + 1, 2), sorted(div.keys()))

parts = plt.violinplot(dists)
for i, pc in enumerate(parts['bodies']):
    if i % 2 == 0:
        pc.set_facecolor('blue')
    else:
        pc.set_facecolor('green')
plt.legend([parts['bodies'][0], parts['bodies'][1]], ['Diverse', 'Baseline'])
plt.savefig('diversity_sampling_jaccard_p50.pdf')
plt.close()

div = defaultdict(list)
ndiv = defaultdict(list)
for row in rows:
    if row['diversity']:
        out = div
    else:
        out = ndiv
    for i, dist in enumerate(row['program-ball-dist']):
        if len(dist) >= row['per-cost']:
            out[i].append(pd.Series(dist).min())

dists = []
for k in sorted(div.keys()):
    dists.append(div[k])
    dists.append(ndiv[k])
plt.title('Diversity sampling (p=%d)' % rows[0]['per-cost'])
plt.ylabel('Min. ball dist. to output')
plt.xlabel('Cost')
plt.xticks(np.arange(1.5, len(dists) + 1, 2), sorted(div.keys()))

parts = plt.violinplot(dists)
for i, pc in enumerate(parts['bodies']):
    if i % 2 == 0:
        pc.set_facecolor('blue')
    else:
        pc.set_facecolor('green')
plt.legend([parts['bodies'][0], parts['bodies'][1]], ['Diverse', 'Baseline'])
plt.savefig('diversity_sampling_ball_p50.pdf')
plt.close()

div = defaultdict(list)
ndiv = defaultdict(list)
for row in rows:
    if row['diversity']:
        out = div
    else:
        out = ndiv
    for i, dist in enumerate(row['program-zs-dist']):
        if len(dist) >= row['per-cost']:
            out[i].append(pd.Series(dist).min())

dists = []
for k in sorted(div.keys()):
    dists.append(div[k])
    dists.append(ndiv[k])
plt.title('Diversity sampling (p=%d)' % rows[0]['per-cost'])
plt.ylabel('Min. Zhang-Sasha dist. to output')
plt.xlabel('Cost')
plt.xticks(np.arange(1.5, len(dists) + 1, 2), sorted(div.keys()))

parts = plt.violinplot(dists)
for i, pc in enumerate(parts['bodies']):
    if i % 2 == 0:
        pc.set_facecolor('blue')
    else:
        pc.set_facecolor('green')
plt.legend([parts['bodies'][0], parts['bodies'][1]], ['Diverse', 'Baseline'])
plt.savefig('diversity_sampling_zs_p50.pdf')

        
