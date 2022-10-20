REMOTE=sketch2.csail.mit.edu
PREFIX=/scratch/feser/

.PHONY: get-runs send-code send-container build-table

send-code:
	cd ..; csail rsync -rL --exclude _build --exclude runs --exclude regel-runs --exclude .direnv symetric bitarray vp-tree combinat $(REMOTE):$(PREFIX)

send-container:
	podman build -t localhost/metric-synth .
	podman save localhost/metric-synth | pv > /tmp/metric-synth.tar
	csail rsync --progress /tmp/metric-synth.tar $(REMOTE):$(PREFIX)
	csail ssh feser@$(REMOTE) "docker load < $(PREFIX)metric-synth.tar"

get-runs:
	rsync -r --progress -zu $(REMOTE):$(PREFIX)symetric/runs/ /mnt/scratch/metric-synth-runs/
