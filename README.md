# Symetric

## Setup

To run the benchmarks and generate plots, you will need the following software installed:
- `docker` (or `podman`, but the makefile uses docker). The makefile assumes that you can run docker without root.
- `python` and `matplotlib`, `pandas`, `numpy` and `tqdm`.

The container build of SyMetric is for *x86-64 linux only*. There is no support for running the benchmarks on macOS on either Intel or ARM. They may work, but we have not tested them in any other environment.

Test that the artifact works by running:
1. `make container` to load the containers.
2. `make bench-towers` to run the benchmarks for the towers domain, which should take ~15 minutes. Output will be produced in `runs/latest`.
3. Create the plot for the towers domain by running `make plots`. This should produce `tower.pdf`, which corresponds to Fig 8b. The number of benchmarks solved may differ depending on the CPU used.

## Running benchmarks

To run the benchmarks in Section 6 of the paper, run the following commands:

1. Load the containers with `make container`.
   2. The SyMetric container can be built with `make build-container`, but you will need to have `nix` installed. `nix` will take care of setting up an OCaml toolchain and building the container. The build process only works on linux.
   3. The Regel container is built from the upstream repository and is provided as-is.
2. Run `make bench`. Benchmarks for the individual domains can be run separately with `make bench-cad`, `make bench-regex`, `make bench-regel`, and `make bench-tower`.
   The cad benchmarks take ~?? hours, the regex benchmarks take ~5 hours, and the tower benchmarks take ~15 minutes, on 20 cores.

   The number of processes is configured at the end of `bin/run_{cad,regex,tower}_bench.xsh`.
   Change the value of the `-j` parameter to `parallel` to increase or decrease the number of processes.
3. Generate Fig. 8 and 10 by running `make plots`.

## Instantiating SyMetric in a new domain

The metric synthesis algorithm is provided as a library written in OCaml.
To instantiate the algorithm with a new domain, provide an implementation of the `Metric_synth.DSL` interface.
Refer to `metric_synth_{cad,regex,tower}.ml` for examples.
Pass the resulting module to `Metric_synth.synthesize` to run the synthesis algorithm.
`Metric_synth.synthesize` also takes:
 - An optional logging function. This function will be called periodically with synthesizer statistics. These statistics can be saved or printed as desired.
 - A record of hyperparameters. The meaning of the hyperparameters is explained in the paper.

`Metric_synth.synthesize` returns a program if successful and `None` if not.
