#!/usr/bin/env python

__doc__ = """
Usage: check_cad_gpt.py [options] <dir>
"""

from docopt import docopt
from glob import glob
import os
import subprocess
import pandas as pd
from tqdm import tqdm


def main(args):
    rows = []
    for out in tqdm(glob(args["<dir>"] + "/*.out")):
        name_parts = os.path.basename(out).split(".")[0].split("-")
        bench_name = name_parts[1]
        repeat = name_parts[2]
        bench_file = glob(f"bench/cad_ext/**/{bench_name}", recursive=True)[0]
        proc = subprocess.run(["_build/default/bin/equiv_cad.exe", bench_file, out])
        rows.append({"bench": bench_name, "repeat": repeat, "result": proc.returncode})
    df = pd.DataFrame(rows)
    print(df)


if __name__ == "__main__":
    main(docopt(__doc__, version="0.1"))
