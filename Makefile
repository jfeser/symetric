REMOTE=sketch4.csail.mit.edu

.PHONY: get-runs send-code send-container build-table

send-code:
	cd ..; csail rsync -rL --progress --exclude _build --exclude runs staged-synth bitarray feser@$(REMOTE):/scratch/feser/

send-container:
	podman build -t localhost/metric-synth .
	podman save localhost/metric-synth | pv > /tmp/metric-synth.tar
	csail rsync --progress /tmp/metric-synth.tar feser@$(REMOTE):/scratch/feser/
	csail ssh feser@$(REMOTE) "docker load < /scratch/feser/metric-synth.tar"

get-runs:
	csail rsync --exclude='*.bin' -r --progress feser@$(REMOTE):/scratch/feser/staged-synth/runs/ runs/
