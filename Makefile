.PHONY: get-runs

send-code:
	cd ..; csail rsync -rL --progress --exclude _build --exclude runs staged-synth feser@sketch4.csail.mit.edu:/scratch/feser/

get-runs:
	csail rsync --exclude='*.bin' -r --progress feser@sketch4.csail.mit.edu:/scratch/feser/staged-synth/runs/ runs/

build-table:
	bin/table.py --main-table ~/metric_synth/main_table.tex --metric-generated runs/2022-03-23-00:48:38/ --beam-generated runs/2022-03-22-02:29:50/ --generated-plot ~/metric_synth/generated.pdf
