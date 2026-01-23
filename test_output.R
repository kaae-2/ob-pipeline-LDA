library(readr)
library(dplyr)
library(stringr)
library(caret)   # for F1 calculation

# Set paths 
test_y_path <- "/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-cytof/out/data/data_import/dataset_name-FR-FCM-Z2KP_virus_final_seed-42/preprocessing/data_preprocessing/num-1_test-sample-limit-5/data_import.test.labels.tar.gz"
pred_y_path <- "/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-cytof/out/data/data_import/.0a2e7b8e58f3cc4a3859f604f164c7ff4a1398ac6dff16c26d45d97b876997c7/preprocessing/data_preprocessing/.7fa4750eefc52d3bd8051ca6a0cc5d9bde7c21938c0d8f384542306fe0569059/analysis/lda/default/data_import_predicted_labels.tar.gz"

# Load data 
test_y_files <- utils::untar(test_y_path, list = TRUE)
test_y_list <- setNames(vector("list", length(test_y_files)), test_y_files)
tmp <- tempdir()
utils::untar(test_y_path, exdir = tmp)

for (file in test_y_files) {
  df <- read_csv(file.path(tmp, file), col_names = FALSE)
  test_y_list[[file]] <- df
}
names(test_y_list)

pred_y_files <- utils::untar(pred_y_path, list = TRUE)
pred_y_list <- setNames(vector("list", length(pred_y_files)), pred_y_files)
tmp <- tempdir()
utils::untar(pred_y_path, exdir = tmp)

for (file in pred_y_files) {
  df <- read_csv(file.path(tmp, file), col_names = FALSE)
  pred_y_list[[file]] <- df
}

names(pred_y_list) <- names(test_y_list)

# F1 score 

f1_results <- data.frame(
  file = character(),
  f1_macro = numeric(),
  f1_micro = numeric(),
  stringsAsFactors = FALSE
)

for (fname in names(test_y_list)) {
  
  print(fname)
  print(length(test_y_list[[fname]]$X1))
  print(length(pred_y_list[[fname]]$X1))
  
  # # ensure match exists
  # if (!fname %in% names(pred_y_list)) {
  #   warning(paste("No prediction file for:", fname))
  #   next
  # }
  # 
  # y_true <- test_y_list[[fname]]$X1 %>% factor()
  # y_pred <- pred_y_list[[fname]]$X1 %>% factor(levels = levels(y_true)) 
  # 
  # levels(y_true)
  # levels(y_pred)
  # # caret::confusionMatrix gives precision/recall per class
  # cm <- caret::confusionMatrix(y_pred, y_true)
  # 
  # # Macro F1 = average of class F1s
  # f1_per_class <- 2 * (cm$byClass[,"Sensitivity"] * cm$byClass[,"Pos Pred Value"]) /
  #   (cm$byClass[,"Sensitivity"] + cm$byClass[,"Pos Pred Value"])
  # 
  # f1_macro <- mean(f1_per_class, na.rm = TRUE)
  # 
  # # Micro F1 = accuracy for multi-class
  # f1_micro <- cm$overall["Accuracy"]
  # 
  # # Store results
  # f1_results <- rbind(
  #   f1_results,
  #   data.frame(file = fname, f1_macro = f1_macro, f1_micro = f1_micro)
  # )
}

print(f1_results)

