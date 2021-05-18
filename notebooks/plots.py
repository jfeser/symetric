#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys

df = pd.read_csv(sys.argv[1], names=['bench', 'synth', 'time', 'max_cost', 'per_cost', 'thresh', 'ball_width', 'bank_size', 'value_dist', 'program_dist', 'program_cost', 'found_program'])
df['program_cost'] = df['program_cost'].replace(-1, np.nan)
print(df.describe())

g = df.groupby(['per_cost', 'ball_width'])
mfp = g.mean()['found_program'].reset_index()

per_cost = mfp['per_cost'].unique()
ball_width = mfp['ball_width'].unique()
values = mfp['found_program'].values.reshape((len(per_cost), len(ball_width)))

fig, ax = plt.subplots()
im = ax.imshow(values)

# We want to show all ticks...
ax.set_xticks(np.arange(len(ball_width)))
ax.set_yticks(np.arange(len(per_cost)))
# ... and label them with the respective list entries
ax.set_xticklabels(ball_width)
ax.set_yticklabels(per_cost)
ax.set_xlabel('Exhaustive search distance')
ax.set_ylabel('States stored per cost')
ax.set_title('Search success rate')

# Loop over data dimensions and create text annotations.
for i in range(len(per_cost)):
    for j in range(len(ball_width)):
        text = ax.text(j, i, values[i, j],
                       ha="center", va="center")

fig.tight_layout()
plt.savefig('success_rate.png')
plt.close()

program_cost = df['program_cost'][df['program_cost'] != -1]
print('Program cost', program_cost.min(), program_cost.median(), program_cost.max())

program_dist = df['program_dist']
print('Program dist', program_dist.min(), program_dist.median(), program_dist.max())

value_dist = df['value_dist']
print('Value dist', value_dist.min(), value_dist.median(), value_dist.max())

mfp = df.groupby(['per_cost', 'ball_width', 'bench']).mean()
bench = df['bench'].unique()

fig, axes = plt.subplots(nrows=len(per_cost), ncols=len(ball_width), sharey=True, sharex=True)
for i, c in enumerate(per_cost):
    for j, w in enumerate(ball_width):
        dat = mfp.loc[(c, w)]['found_program']
        ax = axes[i][j]
        ax.set_title('c=%d, w=%d' % (c, w))
        ax.set_xlabel('Success prob.')
        dat.plot.hist(ax=ax)
plt.show()

mfp = df.groupby(['bench', 'per_cost', 'ball_width']).mean()
print(len(bench))
fig, axes = plt.subplots(figsize=(10, 10), nrows=10, ncols=5, sharey=True, sharex=True, constrained_layout=True)
for b, ax in zip(bench, axes.flat):
    ax.set_title('cost=%d' % mfp.loc[b]['program_cost'].min())
    dat = mfp.loc[b]['found_program'].values.reshape((len(per_cost), len(ball_width)))
    ax.imshow(dat, vmin=0, vmax=1)
plt.show()
