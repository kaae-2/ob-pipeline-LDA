

data_dir="/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-cytof/out/data/data_import/dataset_name-FR-FCM-Z2KP_virus_final_seed-42/preprocessing/data_preprocessing/num-1_test-sample-limit-5"
script_dir="/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-LDA"

Rscript "${script_dir}/ob-pipeline-LDA.R"\
  --name "lda" \
  --output_dir "${script_dir}/out_test" \
  --data.train_matrix "${data_dir}/data_import.train.matrix.tar.gz" \
  --data.train_labels "${data_dir}/data_import.train.labels.tar.gz" \
  --data.test_matrix "${data_dir}/data_import.test.matrices.tar.gz" 
  
  