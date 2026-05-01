#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "$0")" && pwd)"
input_dir="${script_dir}/out/data/data_preprocessing/default"
output_dir="${script_dir}/out/data/analysis/default/lda"

mkdir -p "$output_dir"

Rscript "${script_dir}/ob-pipeline-LDA.R" \
  --name "lda" \
  --output_dir "$output_dir" \
  --data.train_matrix "${input_dir}/data_import.train.matrix.tar.gz" \
  --data.train_labels "${input_dir}/data_import.train.labels.tar.gz" \
  --data.test_matrix "${input_dir}/data_import.test.matrices.tar.gz"
