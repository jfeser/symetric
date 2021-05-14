#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import sys

df = pd.read_csv(sys.argv[1], names=['dist', 'pdist'])
df = df.pivot(columns='pdist', values=['dist']).reindex()

cols = [list(df['dist'][float(i)].dropna()) for i in range(11)]
print(cols)
fig, ax = plt.subplots()
ax.set_title('Search space distribution')
ax.set_xlabel('Tree edit distance to closest solution')
ax.set_ylabel('Hamming distance to solution')
ax.violinplot(cols)
plt.savefig('dist.png')

