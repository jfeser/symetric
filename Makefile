all: build

build:
	dune build

container:
	nix build .#buildContainer
	./result | podman load
	rm result

send-code:
	cd ..; csail rsync -rL --exclude _build --exclude runs --exclude regel-runs --exclude .direnv symetric bitarray vp-tree combinat $(REMOTE):$(PREFIX)/ocaml-workspace/

send-container:
	podman build -t localhost/metric-synth .
	podman save localhost/metric-synth | pv > /tmp/metric-synth.tar
	csail rsync --progress /tmp/metric-synth.tar $(REMOTE):$(PREFIX)
	csail ssh $(REMOTE) "docker load < $(PREFIX)metric-synth.tar"

get-runs:
	rsync -r --progress -zu $(REMOTE):$(PREFIX)/ocaml-workspace/symetric/runs/ /mnt/scratch/metric-synth-runs/
