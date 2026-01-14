## ============================================================
## 0. Install dependencies
## ============================================================
if (!require("flowCore")) install.packages("flowCore")
if (!require("MASS")) install.packages("MASS")
if (!require("devtools")) install.packages("devtools")

library(flowCore)
library(MASS)
library(devtools)
library(readr)
library(dplyr)
library(glue)

## ============================================================
## 1. Load CyTOF-Linear-Classifier functions from GitHub
## ============================================================
source_url("https://raw.githubusercontent.com/tabdelaal/CyTOF-Linear-Classifier/master/CyTOF_LDAtrain.R")
source_url("https://raw.githubusercontent.com/tabdelaal/CyTOF-Linear-Classifier/master/CyTOF_LDApredict.R")

## ============================================================
## 2. Specify paths to your data
##    Wrangle format at location to fit with the tool 
## ============================================================

dataset_path <- "/home/projects/dp_immunoth/data/benchmark_flow/FR-FCM-Z2KP/healthy_final"

# List all files in dataset
files <- list.files(path = dataset_path, pattern = '.csv',full.names = TRUE)
file_names <- lapply(files, basename)

# Extract the first file as training sample and the rest as test.
train_path <- files[[1]]
test_paths <- files[c(2:length(files))]

# Load files - only first 10000 lines of each file as test, use full dataset for real run
train <- read.csv(file = train_path, nrows = 10000)
test_list <- lapply(test_paths, function(x) read.csv(file = x, nrows = 10000))

# Extract training set (without labels)
train_x <- train[,!(colnames(train) %in% c("label", "cell_id"))]

# Extract training labels
train_y <- train$label %>% as.data.frame()

# Extract test set (without labels)
test_x_list <- lapply(test_list, function(df) {
  df[, !(colnames(df) %in% c("label", "cell_id"))]
})
names(test_x_list) <- file_names[2:length(files)]

# Extract test labels
test_y_list <- lapply(test_list, function(df) {
  df$label
})
names(test_y_list) <- file_names[2:length(files)]

# RelevantMarkers needed for function 
RelevantMarkers_char <- colnames(train_x)
names(RelevantMarkers_char) <- 1:length(RelevantMarkers_char)
RelevantMarkers <- names(RelevantMarkers_char) %>% as.integer()

# Specify paths - should be somewhere in the benchmark - tpm folder?
TrainingSamplesExt <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tpm/TrainingSamplesExt"
TrainingLabelsExt <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tpm/TrainingLabelsExt"
TestingSamplesExt <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tpm/TestingSamplesExt"

dirs <- c(TrainingSamplesExt, TrainingLabelsExt, TestingSamplesExt)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  } else {
    message("Exists: ", d)
  }
}

# Save without header
write_delim(
  x = train_x, 
  file = paste0(TrainingSamplesExt, "/train_x.csv"), 
  col_names = FALSE,
  delim = ","
)

write_delim(
  x = train_y, 
  file = paste0(TrainingLabelsExt, "/train_y.csv"), 
  col_names = FALSE,
  delim = ","
)

invisible(
  lapply(names(test_x_list), function(nm) {
    write_delim(
      x = test_x_list[[nm]],
      file = file.path(TestingSamplesExt, paste0("test_x_", nm)), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)

# list.files(path = "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tpm/TrainingSamplesExt", pattern = '.csv',full.names = TRUE)

## ============================================================
## 4. Train LDA model
## ============================================================

cat("Training LDA model…\n")

LDAclassifier <- CyTOF_LDAtrain(
  TrainingSamplesExt = TrainingSamplesExt,
  TrainingLabelsExt = TrainingLabelsExt,
  mode = "CSV",
  RelevantMarkers = RelevantMarkers,
  # LabelIndex = FALSE,
  Transformation = FALSE
)

## ============================================================
## 5. Predict labels on test data
## ============================================================
cat("Predicting…\n")

RejectionThreshold <- 0.7 # set to something OR parameter

pred_labels_all <- CyTOF_LDApredict(
  Model = LDAclassifier,
  TestingSamplesExt = TestingSamplesExt,
  mode = "CSV",
  RejectionThreshold = RejectionThreshold
)

names(pred_labels_all) <- names(test_y_list)

# Delete tpm folder now that we used it
unlink("/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tpm/", recursive = TRUE)

# Export pred_labels
# MAKE THIS OMNIBENCHMARK FRIENDLY
out_dir <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/LDA_predictions/"

for (sample_name in names(test_y_list)){
  
  # sample_name <- names(test_y_list)[[1]]
  out_sample_name <- glue("LDA_predicted_{sample_name}")
  pred_labels <- pred_labels_all[[sample_name]]
  test_y_char <- test_y_list[[sample_name]]
  
  if (length(pred_labels) != length(test_y_char)) {
    
    message("Predicted and real test labels are of different lengths.")
    message(glue("Predicted labels have {length(pred_labels)} entries."))
    message(glue("Real labels have {length(test_y_char)} entries."))
    
  } else {
    message("Predicted and real test labels are of same lengths. Saving predicted labels...")
    
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    } 
    
    write_delim(
      x = train_y, 
      file = paste0(out_dir, out_sample_name), 
      col_names = FALSE,
      delim = ","
    )
    
  }
  
  acc <- mean(pred_labels == test_y_char)
  print(sample_name)
  print(acc)
  
}


## ============================================================
## 6. Evaluate performance (if true labels available)
## ============================================================

# cat("\nAccuracy:\n")
# acc <- mean(pred_labels == test_y_char)
# print(acc)
# 
# cat("\nConfusion matrix:\n")
# print(table(Predicted = pred_labels, True = test_y_char))
