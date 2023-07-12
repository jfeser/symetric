RUNS=runs/$(shell date '+%Y-%m-%d-%H:%M:%S')
REMOTE=sketch3.csail.mit.edu
PREFIX=/scratch/feser/

all: build

.PHONY: bench build container

build:
	dune build

build-container:
	nix build .#buildContainer
	./result > containers/symetric.tar

container:
	sudo docker load < containers/symetric.tar
	sudo docker load < containers/regel.tar

bench: bench-cad bench-regex bench-tower

bench-cad:
	mkdir -p latest
	sudo docker run --it --rm -v $(shell pwd):/work localhost/jfeser/symetric:latest \
		bash -c "mkdir -p /tmp; xonsh bin/run_cad_bench.xsh latest"

bench-regex:
	mkdir -p latest
	sudo docker run --it --rm -v $(shell pwd):/work localhost/jfeser/symetric:latest \
		bash -c "mkdir -p /tmp; xonsh bin/run_regex_bench.xsh latest"

bench-tower:
	mkdir -p latest
	sudo docker run --it --rm -v $(shell pwd):/work localhost/jfeser/symetric:latest \
		bash -c "mkdir -p /tmp; xonsh bin/run_tower_bench.xsh latest"

send-code:
	cd ..; rsync -rL --exclude _build --exclude runs --exclude regel-runs --exclude .direnv symetric $(REMOTE):$(PREFIX)/ocaml-workspace/

send-container: container
	podman save jfeser/symetric:latest | pv > /tmp/symetric.tar
	rsync --progress /tmp/symetric.tar $(REMOTE):$(PREFIX)
	ssh $(REMOTE) "docker load < $(PREFIX)/symetric.tar"

get-runs:
	rsync -r --progress -zu $(REMOTE):$(PREFIX)/ocaml-workspace/symetric/runs/ runs/
