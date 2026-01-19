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
library(purrr)
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

# Path to zipped data
# dataset_path <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/prep_data/out"

# GET ARGUMENTS 
parser <- ArgumentParser(description="FCPS caller")

# parser$add_argument('--data.matrix',
#                     type="character",
#                     help='gz-compressed textfile containing the comma-separated data to be clustered.')
# parser$add_argument('--data.true_labels',
#                     type="character",
#                     help='gz-compressed textfile with the true labels.')
parser$add_argument('--train.data.matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
parser$add_argument('--labels_train',
                    type="character",
                    help='gz-compressed textfile with the true labels.')
parser$add_argument('--test.data.matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
parser$add_argument('--labels_test',
                    type="character",
                    help='gz-compressed textfile with the true labels.')
parser$add_argument('--seed',
                    type="integer",
                    help='Random seed',
                    default = 819797,
                    dest = 'seed')
parser$add_argument("--output_dir", "-o", dest="output_dir", type="character",
                    help="output directory where files will be saved", default=getwd())
parser$add_argument("--name", "-n", dest="name", type="character", help="name of the dataset")
# parser$add_argument("--method", "-m", dest="method", type="character", help="method")

args <- parser$parse_args()


# LOAD TRAINING X
# train_x_path <- glue("{dataset_path}/train_x.zip")
train_x_path <- args[['train.data.matrix']]
train_x_zip_contents <- unzip(train_x_path, list = TRUE)$Name
csv_files <- train_x_zip_contents[grepl("\\.csv$", train_x_zip_contents)]

train_x_list <- lapply(csv_files, function(f) {
  read.csv(unz(train_x_path, f))
})

# LOAD TRAINING Y
train_y_path <- glue("{dataset_path}/train_y.zip")
train_y_path <- args[['labels_train']]
train_y_zip_contents <- unzip(train_y_path, list = TRUE)$Name
csv_files <- train_y_zip_contents[grepl("\\.csv$", train_y_zip_contents)]

train_y_list <- lapply(csv_files, function(f) {
  read.csv(unz(train_y_path, f))
})

# LOAD TEST X
# test_x_path <- glue("{dataset_path}/test_x.zip")
test_x_path <- args[['test.data.matrix']]
test_x_zip_contents <- unzip(test_x_path, list = TRUE)$Name
csv_files <- test_x_zip_contents[grepl("\\.csv$", test_x_zip_contents)]

test_x_list <- lapply(csv_files, function(f) {
  read.csv(unz(test_x_path, f))
})

# # LOAD TEST Y
# test_y_path <- glue("{dataset_path}/test_y.zip")
# test_y_zip_contents <- unzip(test_y_path, list = TRUE)$Name
# csv_files <- test_y_zip_contents[grepl("\\.csv$", test_y_zip_contents)]
# 
# test_y_list <- lapply(csv_files, function(f) {
#   read.csv(unz(test_y_path, f))
# })

# RelevantMarkers needed for function 
RelevantMarkers_char <- colnames(train_x_list[[1]]) # ASSUMING TRAINING SAMPLES HAVE SAME COLUMNS
names(RelevantMarkers_char) <- 1:length(RelevantMarkers_char)
RelevantMarkers <- names(RelevantMarkers_char) %>% as.integer()

# Specify paths - should be somewhere in the benchmark - tmp folder?
TrainingSamplesExt <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tmp_LDA/TrainingSamplesExt"
TrainingLabelsExt <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tmp_LDA/TrainingLabelsExt"
TestingSamplesExt <- "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tmp_LDA/TestingSamplesExt"

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
invisible(
  lapply(1:length(train_x_list), function(nm) {
    write_delim(
      x = train_x_list[[nm]],
      file = file.path(TrainingSamplesExt, glue("train_x_{nm}.csv")), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)

invisible(
  lapply(1:length(train_y_list), function(nm) {
    write_delim(
      x = train_y_list[[nm]],
      file = file.path(TrainingLabelsExt, glue("train_y_{nm}.csv")), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)

invisible(
  lapply(1:length(test_x_list), function(nm) {
    write_delim(
      x = test_x_list[[nm]],
      file = file.path(TestingSamplesExt, glue("test_x_{nm}.csv")), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)


# list.files(path = "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tmp/TrainingSamplesExt", pattern = '.csv',full.names = TRUE)

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

## ============================================================
## 6. Export labels 
## ============================================================ 

export_list_as_zipped_csv <- function(lst, zip_path) {
  
  file_names <- sprintf("%s_%d.csv", "labels", seq_along(lst))
  
  # Write the CSVs to the working directory
  for (i in seq_along(lst)) {
    df <- data.frame(label = lst[[i]])
    write.csv(df, file_names[i], row.names = FALSE)
  }
  
  # Use system zip (same as right-click compress)
  system(sprintf("zip -j %s %s", zip_path, paste(file_names, collapse = " ")))
  
  return(zip_path)
}

# export_list_as_zipped_csv(pred_labels_all, "/home/projects/dp_immunoth/people/helweg/projects/benchmarking/ob-pipeline-LDA/out/labels.zip")
export_list_as_zipped_csv(pred_labels_all, args[['output_dir']])


# Delete tmp folder now that we used it
unlink("/home/projects/dp_immunoth/people/helweg/projects/benchmarking/tmp_LDA/", recursive = TRUE)
