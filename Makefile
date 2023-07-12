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
	rm result

container:
	docker load < containers/symetric.tar
	docker load < containers/regel.tar

bench: bench-cad bench-regex bench-tower bench-regel

bench-cad:
	mkdir -p runs/latest
	docker run -it --rm -v $(shell pwd):/work localhost/jfeser/symetric:latest \
		sh -c "mkdir -p /tmp; xonsh bin/run_cad_bench.xsh /work/runs/latest"

bench-regex:
	mkdir -p runs/latest
	docker run -it --rm -v $(shell pwd):/work localhost/jfeser/symetric:latest \
		sh -c "mkdir -p /tmp; xonsh bin/run_regex_bench.xsh /work/runs/latest"

bench-regel:
	mkdir -p runs/latest
	docker run -it rm -v $(shell pwd):/work localhost/regel:latest \
		sh -c "cp /work/bin/regel_commands.txt /work/bin/parallel .; ant -buildfile resnax/build.xml resnax; mkdir -p /home/regel/exp/so/log; ./parallel --timeout 300 --eta -j 4 --joblog regel_joblog :::: regel_commands.txt; cp -r /home/regel/exp/so/log /work/runs/latest/regel"

bench-tower:
	mkdir -p runs/latest
	docker run -it --rm -v $(shell pwd):/work localhost/jfeser/symetric:latest \
		sh -c "mkdir -p /tmp; xonsh bin/run_tower_bench.xsh /work/latest"

plots:
	python bin/plot.py

send-code:
	cd ..; rsync -rL --progress --exclude _build --exclude runs --exclude regel-runs --exclude .direnv symetric $(REMOTE):$(PREFIX)/ocaml-workspace/

send-container: build-container
	podman save jfeser/symetric:latest | pv > /tmp/symetric.tar
	rsync --progress /tmp/symetric.tar $(REMOTE):$(PREFIX)
	ssh $(REMOTE) "sudo docker load < $(PREFIX)/symetric.tar"

get-runs:
	rsync -r --progress -zu $(REMOTE):$(PREFIX)/ocaml-workspace/symetric/runs/ runs/
