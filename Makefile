REMOTE=ec2-user@ec2-100-24-115-131.compute-1.amazonaws.com

.PHONY: get-runs send-code send-container build-table

send-code:
	cd ..; rsync -e "ssh -i /home/feser/.ssh/aws-popl2023.pem" -rL --exclude _build --exclude runs --exclude regel-runs staged-synth bitarray $(REMOTE):

send-container:
	podman build -t localhost/metric-synth .
	podman save localhost/metric-synth | pv > /tmp/metric-synth.tar
	rsync -e "ssh -i /home/feser/.ssh/aws-popl2023.pem" --progress /tmp/metric-synth.tar $(REMOTE):
	# csail ssh feser@$(REMOTE) "docker load < /scratch/feser/metric-synth.tar"

get-runs:
	rsync -e "ssh -i /home/feser/.ssh/aws-popl2023.pem" -r --progress -zu $(REMOTE):staged-synth/runs/ runs/
