Using Miniconda3 Python (3.7) with the following sourced from `~/.bash_profile`

	export PATH="/Users/mjames/miniconda3/bin:$PATH"

Run the following to render a local copy of these markdown documents as Material Design elements:

	conda env create -f environment.yml
	source activate gempak-docs
	mkdocs serve
