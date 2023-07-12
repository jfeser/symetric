RUNS=runs/$(shell date '+%Y-%m-%d-%H:%M:%S')
REMOTE=sketch3.csail.mit.edu
PREFIX=/scratch/feser/

all: build

.PHONY: bench build container

build:
	dune build

container:
	nix build .#buildContainer
	./result | podman load
	rm result

bench: bench-cad bench-regex bench-tower

bench-cad:
	mkdir -p /tmp
	mkdir -p $(RUNS)
	xonsh bin/run_cad_bench.xsh $(RUNS)

bench-regex:
	mkdir -p /tmp
	mkdir -p $(RUNS)
	xonsh bin/run_regex_bench.xsh $(RUNS)

bench-tower:
	mkdir -p /tmp
	mkdir -p $(RUNS)
	xonsh bin/run_tower_bench.xsh $(RUNS)

plots:
	python bin/plot.py

send-code:
	cd ..; rsync -rL --exclude _build --exclude runs --exclude regel-runs --exclude .direnv symetric $(REMOTE):$(PREFIX)/ocaml-workspace/

send-container: container
	podman save jfeser/symetric:latest | pv > /tmp/symetric.tar
	rsync --progress /tmp/symetric.tar $(REMOTE):$(PREFIX)
	ssh $(REMOTE) "docker load < $(PREFIX)/symetric.tar"

get-runs:
	rsync -r --progress -zu $(REMOTE):$(PREFIX)/ocaml-workspace/symetric/runs/ runs/
