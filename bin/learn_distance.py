#!/usr/bin/env python3

from metric_learn import LMNN, LFDA, SDML_Supervised
from dml import NCA
import numpy as np
import sys

def read_inputs():
    X = np.lib.format.read_array(sys.stdin)
    y = np.lib.format.read_array(sys.stdin)

def main(feat_fn, labels_fn):
    with open(feat_fn, 'rb') as f:
        X = np.lib.format.read_array(f)
    with open(labels_fn, 'rb') as f:
        y = np.lib.format.read_array(f)
        
    # lmnn = LMNN(verbose=True)
    # lmnn.fit(X, y)
    # np.lib.format.write_array(sys.stdout, lmnn.get_mahalanobis_matrix())    

    # lfda = LFDA(n_components=10)
    # lfda.fit(X, y)

    # learner = SDML_Supervised(verbose=True)
    learner = NCA(verbose=True)
    learner.fit(X[:, :15], y)
    # lmnn.fit(X, y)
    # np.lib.format.write_array(sys.stdout, lmnn.get_mahalanobis_matrix())    

    
if __name__ == '__main__':
    if len(sys.argv) != 3:
        print('Usage: learn_distance.py FEATURES LABELS')
        exit(-1)
        
    main(sys.argv[1], sys.argv[2])
