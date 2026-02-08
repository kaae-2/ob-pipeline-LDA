## ============================================================
## 0. Install dependencies
## ============================================================
# if (!require("flowCore")) install.packages("flowCore")
# if (!require("MASS")) install.packages("MASS")
# if (!require("devtools")) install.packages("devtools")

cat("Loading tools...")

library(argparse)
library(glue)
library(readr)
library(dplyr)
library(utils)

# setwd("~/Documents/courses/Benchmarking/repos/ob-pipeline-LDA/")

## ============================================================
## 1. Load CyTOF-Linear-Classifier functions (vendored)
## ============================================================
# Vendored from https://github.com/tabdelaal/CyTOF-Linear-Classifier
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- cmd_args[startsWith(cmd_args, file_arg)]
  if (length(match) == 0) {
    return(getwd())
  }
  script_path <- sub(file_arg, "", match[[1]])
  dirname(normalizePath(script_path))
}

script_dir <- get_script_dir()
vendor_dir <- file.path(script_dir, "vendor")

source(file.path(vendor_dir, "CyTOF_LDAtrain.R"))
source(file.path(vendor_dir, "CyTOF_LDApredict.R"))


## ============================================================
## 2. Specify paths to your data
##    Wrangle format at location to fit with the tool 
## ============================================================

cat("Getting arguments...")
# GET ARGUMENTS 
parser <- ArgumentParser(description="FCPS caller")

parser$add_argument('--data.train_matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
parser$add_argument('--data.train_labels',
                    type="character",
                    help='gz-compressed textfile with the true labels.')
parser$add_argument('--data.test_matrix',
                    type="character",
                    help='gz-compressed textfile containing the comma-separated data to be clustered.')
# parser$add_argument('--data.test_labels',
#                     type="character",
#                     help='gz-compressed textfile with the true labels.')
parser$add_argument("--output_dir", "-o", dest="output_dir", type="character",
                    help="output directory where files will be saved", default=getwd())
parser$add_argument("--name", "-n", dest="name", type="character", help="name of the dataset")
# parser$add_argument("--method", "-m", dest="method", type="character", help="method")

args <- parser$parse_args()

# Prepare a unique temp workspace under output dir to avoid collisions and /tmp limits
output_dir <- args[['output_dir']]
base_tmp <- file.path(
  output_dir,
  paste0("tmp_LDA_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%S"))
)
dir.create(base_tmp, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(base_tmp, recursive = TRUE), add = TRUE)

# FOR TESTING
# Path to zipped data
# dataset_path <- "/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-cytof/out/data/data_import/dataset_name-FR-FCM-Z2KP_virus_final_seed-42/preprocessing/data_preprocessing/num-1_test-sample-limit-5"
# train_x_path <- glue("{dataset_path}/data_import.train.matrix.tar.gz")
# train_y_path <- glue("{dataset_path}/data_import.train.labels.tar.gz")
# # test_y_path <- glue("{dataset_path}/data_import.test.labels.tar.gz")
# test_x_path <- glue("{dataset_path}/data_import.test.matrices.tar.gz")

cat("Loading data...")
train_x_path <- args[['data.train_matrix']]
train_y_path <- args[['data.train_labels']]
test_x_path <- args[['data.test_matrix']]

get_sample_number <- function(file_name, fallback) {
  base <- basename(file_name)
  base <- gsub("\\.csv(\\.gz)?$", "", base)
  m <- regexpr("[0-9]+(?!.*[0-9])", base, perl = TRUE)
  if (m[1] == -1) {
    return(as.character(fallback))
  }
  substr(base, m[1], m[1] + attr(m, "match.length") - 1)
}

extract_archive <- function(path, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::untar(path, exdir = out_dir)
}

list_csv_files <- function(path) {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  files[order(basename(files))]
}

read_csv_no_header <- function(path) {
  read_csv(path, col_names = FALSE, show_col_types = FALSE)
}

# ---------------------------
# Specify paths - unique tmp folder under output dir
# ---------------------------
TrainingSamplesExt <- file.path(base_tmp, "TrainingSamplesExt")
TrainingLabelsExt <- file.path(base_tmp, "TrainingLabelsExt")
TestingSamplesExt <- file.path(base_tmp, "TestingSamplesExt")

ExtractTrainX <- file.path(base_tmp, "extract_train_x")
ExtractTrainY <- file.path(base_tmp, "extract_train_y")
ExtractTestX <- file.path(base_tmp, "extract_test_x")

dirs <- c(
  TrainingSamplesExt,
  TrainingLabelsExt,
  TestingSamplesExt,
  ExtractTrainX,
  ExtractTrainY,
  ExtractTestX
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  } else {
    message("Exists: ", d)
  }
}

extract_archive(train_x_path, ExtractTrainX)
extract_archive(train_y_path, ExtractTrainY)
extract_archive(test_x_path, ExtractTestX)

train_x_files <- list_csv_files(ExtractTrainX)
train_y_files <- list_csv_files(ExtractTrainY)
test_x_files <- list_csv_files(ExtractTestX)

if (length(train_x_files) == 0) {
  stop("No training matrix CSV files found after extraction.")
}
if (length(train_y_files) == 0) {
  stop("No training label CSV files found after extraction.")
}
if (length(test_x_files) == 0) {
  stop("No test matrix CSV files found after extraction.")
}

x_names <- basename(train_x_files)
y_names <- basename(train_y_files)
x_ids <- vapply(seq_along(x_names), function(i) get_sample_number(x_names[i], i), character(1))
y_ids <- vapply(seq_along(y_names), function(i) get_sample_number(y_names[i], i), character(1))
y_id_counts <- table(y_ids)
use_id_match <- all(x_ids %in% y_ids) && all(y_id_counts == 1)

if (!use_id_match && length(train_x_files) != length(train_y_files)) {
  stop(glue(
    "Training X/Y file count mismatch and sample-id matching failed: x={length(train_x_files)} y={length(train_y_files)}"
  ))
}

marker_count <- NULL
for (i in seq_along(train_x_files)) {
  x_file <- train_x_files[[i]]
  y_file <- if (use_id_match) {
    matched <- match(x_ids[i], y_ids)
    if (is.na(matched)) {
      stop(glue("Missing labels for training sample id {x_ids[i]} ({basename(x_file)})"))
    }
    train_y_files[[matched]]
  } else {
    train_y_files[[i]]
  }

  x <- read_csv_no_header(x_file)
  y <- read_csv_no_header(y_file)
  y <- y[, 1, drop = FALSE]

  if (nrow(x) != nrow(y)) {
    stop(glue(
      "Length mismatch for {basename(x_file)} and {basename(y_file)}: data={nrow(x)} labels={nrow(y)}"
    ))
  }

  x_matrix <- as.matrix(data.frame(lapply(x, function(col) suppressWarnings(as.numeric(col)))))
  valid_rows <- complete.cases(x_matrix) & apply(is.finite(x_matrix), 1, all)

  y_num <- suppressWarnings(as.numeric(as.character(y[[1]])))
  unlabeled_mask <- is.na(y_num) | y_num == 0
  keep <- valid_rows & !unlabeled_mask

  if (!any(keep)) {
    stop(glue("Training sample {basename(x_file)} has no valid labeled rows after filtering."))
  }

  x_clean <- as.data.frame(x_matrix[keep, , drop = FALSE])
  y_clean <- data.frame(V1 = y_num[keep])

  if (is.null(marker_count)) {
    marker_count <- ncol(x_clean)
  } else if (ncol(x_clean) != marker_count) {
    stop(glue(
      "Training marker count mismatch: expected {marker_count}, got {ncol(x_clean)} for {basename(x_file)}"
    ))
  }

  out_name <- glue("LDA_{basename(x_file)}")
  write_delim(
    x = x_clean,
    file = file.path(TrainingSamplesExt, out_name),
    col_names = FALSE,
    delim = ","
  )
  write_delim(
    x = y_clean,
    file = file.path(TrainingLabelsExt, out_name),
    col_names = FALSE,
    delim = ","
  )
}

if (is.null(marker_count) || marker_count < 1) {
  stop("No valid training markers found after filtering.")
}

for (file in test_x_files) {
  out_name <- glue("LDA_{basename(file)}")
  ok <- file.copy(file, file.path(TestingSamplesExt, out_name), overwrite = TRUE)
  if (!ok) {
    stop(glue("Failed to stage test file {basename(file)}"))
  }
}

RelevantMarkers <- seq_len(marker_count)

## ============================================================
## 3. Train LDA model
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
## 4. Predict labels on test data
## ============================================================
cat("Predicting…\n")

RejectionThreshold <- 0.7 # parameter, set to 0.7 in example and is hence interpreted and default.

pred_labels_all <- CyTOF_LDApredict(
  Model = LDAclassifier,
  TestingSamplesExt = TestingSamplesExt,
  mode = "CSV",
  RejectionThreshold = RejectionThreshold
)

names(pred_labels_all) <- list.files(TestingSamplesExt)

# pred_labels_all$`LDA_data_import-data-10.csv` %>% length()
# test_x_list$`data_import-data-10.csv` %>% dim()

## ============================================================
## 5. Export labels 
## ============================================================ 

# Create a temporary folder to store CSVs
# output_dir <- "./out_test/"
tmp_dir <- file.path(base_tmp, "predictions_tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
csv_files <- character(length(pred_labels_all))

# Loop through the list and write each CSV
i <- 1
for (name in names(pred_labels_all)) {
  
  csv_file <- file.path(tmp_dir, name)
  
  # If the element is a data.frame or list, coerce to data.frame
  df <- as.data.frame(pred_labels_all[[name]])
  
  write_delim(df, file = csv_file, col_names = FALSE, quote = "none", delim = ",")
  csv_files[i] <- csv_file
  i <- i + 1
  
}

# Create tar.gz archive of all CSVs
name <- args[['name']]
tar(tarfile = glue("{output_dir}/{name}_predicted_labels.tar.gz"), files = csv_files, compression = "gzip", tar = "internal")


# Temporary workspace is cleaned by on.exit.
