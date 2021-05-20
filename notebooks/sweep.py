#!/usr/bin/env python3

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys

df = pd.read_csv(sys.argv[1], header=None)
print(df[df[1] > 0][3].describe())
print(df[df[1] < 1][3].describe())
