#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
from scipy.stats import ttest_ind
from collections import defaultdict
import math
import ujson

df = pd.read_json(sys.argv[1], orient='records')
print(df.describe())
df.groupby(['bench', 'synth']).plot(kind='box')
plt.show()
