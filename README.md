# LDA Module

## What this module does

Runs an LDA-based classifier on preprocessed train/test tar archives.

- Script: `ob-pipeline-LDA.R`
- Local helper script: `run_LDA.sh`
- Output: `lda_predicted_labels.tar.gz`

The current wrapper trains on filtered labeled rows (`label != 0`) and predicts
test labels per sample.

## Run locally

Preferred (portable):

```bash
Rscript models/LDA/ob-pipeline-LDA.R --name lda --output_dir models/LDA/out/data/analysis/default/lda --data.train_matrix <train.matrix.tar.gz> --data.train_labels <train.labels.tar.gz> --data.test_matrix <test.matrices.tar.gz>
```

The checked-in `run_LDA.sh` is currently machine-specific and may require local
path edits before use.

## Run as part of benchmark

Configured in `benchmark/Clustering_conda.yml` analysis stage; run via:

```bash
just benchmark
```

## What `run_LDA.sh` / `ob-pipeline-LDA.R` need

- `Rscript` in `PATH`
- R packages used by the wrapper (including `MASS`, `argparse`, `data.table`)
- Preprocessing train/test tar inputs
- Writable output directory
