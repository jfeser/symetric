#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
from scipy.stats import ttest_ind

df = pd.read_csv(sys.argv[1])
print(df.describe())

ndiv = df[df['diversity'] == 0]
div = df[df['diversity'] == 1]
pdf = pd.DataFrame(data={'ndiv': (ndiv['have-parts'] / ndiv['total-parts']),
                         'div': (div['have-parts'] / div['total-parts'])
                         })
pdf.plot(kind='box')
plt.show()

print(pdf['ndiv'].describe())
print(pdf['div'].describe())
print(ttest_ind(pdf['ndiv'].dropna(), pdf['div'].dropna()))

ndiv = df[df['diversity'] == 0]
div = df[df['diversity'] == 1]
pdf = pd.DataFrame(data={'ndiv': ndiv['closest-program'],
                         'div': div['closest-program']
                         })
pdf.plot(kind='box')
plt.show()

print(pdf['ndiv'].describe())
print(pdf['div'].describe())
print(ttest_ind(pdf['ndiv'].dropna(), pdf['div'].dropna()))
