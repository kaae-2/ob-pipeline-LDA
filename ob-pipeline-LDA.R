## ============================================================
## 0. Install dependencies
## ============================================================
# if (!require("flowCore")) install.packages("flowCore")
# if (!require("MASS")) install.packages("MASS")
# if (!require("devtools")) install.packages("devtools")

cat("Loading tools...")

library(argparse)
library(data.table)
library(glue)
library(readr)
library(dplyr)
library(utils)

# setwd("~/Documents/courses/Benchmarking/repos/ob-pipeline-LDA/")

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
parser$add_argument('--data.label_key',
                    type="character",
                    help='label key metadata path (accepted but unused).')
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
  data.table::fread(path, header = FALSE, data.table = FALSE, showProgress = FALSE)
}

read_label_no_header <- function(path) {
  label_df <- data.table::fread(
    path,
    header = FALSE,
    data.table = FALSE,
    select = 1,
    showProgress = FALSE
  )
  suppressWarnings(as.numeric(label_df[[1]]))
}

train_lda_model <- function(training_data, training_labels, transformation, markers) {
  if (transformation != FALSE) {
    if (transformation == "arcsinh") {
      training_data <- asinh(training_data / 5)
    } else if (transformation == "log") {
      training_data <- log(training_data)
      training_data[sapply(training_data, is.infinite)] <- 0
    }
  }

  classifier <- MASS::lda(training_data, as.factor(training_labels))
  list(LDAclassifier = classifier, Transformation = transformation, markers = markers)
}

predict_labels_for_file <- function(model, csv_path, rejection_threshold) {
  temp <- data.table::fread(csv_path, header = FALSE, data.table = FALSE, showProgress = FALSE)
  testing_data <- temp[, model$markers, drop = FALSE]
  rm(temp)

  if (model$Transformation != FALSE) {
    if (model$Transformation == "arcsinh") {
      testing_data <- asinh(testing_data / 5)
    } else if (model$Transformation == "log") {
      testing_data <- log(testing_data)
      testing_data[sapply(testing_data, is.infinite)] <- 0
    }
  }

  predictions <- predict(model$LDAclassifier, testing_data)
  post_max <- apply(predictions$posterior, 1, max)
  classes <- as.character(predictions$class)
  classes[post_max < rejection_threshold] <- "0"

  rm(testing_data, predictions, post_max)
  invisible(gc(verbose = FALSE))
  classes
}

# ---------------------------
# Specify paths - unique tmp folder under output dir
# ---------------------------
ExtractTrainX <- file.path(base_tmp, "extract_train_x")
ExtractTrainY <- file.path(base_tmp, "extract_train_y")
ExtractTestX <- file.path(base_tmp, "extract_test_x")

dirs <- c(
  ExtractTrainX,
  ExtractTrainY,
  ExtractTestX
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
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
train_feature_chunks <- vector("list", length(train_x_files))
train_label_chunks <- vector("list", length(train_x_files))
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
  y_num <- read_label_no_header(y_file)

  if (nrow(x) != length(y_num)) {
    stop(glue(
      "Length mismatch for {basename(x_file)} and {basename(y_file)}: data={nrow(x)} labels={length(y_num)}"
    ))
  }

  x_matrix <- as.matrix(x)
  valid_rows <- rowSums(!is.finite(x_matrix)) == 0

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

  train_feature_chunks[[i]] <- x_clean
  train_label_chunks[[i]] <- y_clean[[1]]
}

if (is.null(marker_count) || marker_count < 1) {
  stop("No valid training markers found after filtering.")
}

RelevantMarkers <- seq_len(marker_count)
training_data <- do.call(rbind, train_feature_chunks)
training_labels <- unlist(train_label_chunks, use.names = FALSE)

## ============================================================
## 3. Train LDA model
## ============================================================

cat("Training LDA model…\n")

LDAclassifier <- train_lda_model(
  training_data = training_data,
  training_labels = training_labels,
  transformation = FALSE,
  markers = RelevantMarkers
)

rm(training_data, training_labels, train_feature_chunks, train_label_chunks)
invisible(gc(verbose = FALSE))

## ============================================================
## 4. Predict labels on test data
## ============================================================
cat("Predicting…\n")

RejectionThreshold <- 0.7 # parameter, set to 0.7 in example and is hence interpreted and default.

prediction_files <- test_x_files
if (length(prediction_files) == 0) {
  stop("No test CSV files found for prediction.")
}

## ============================================================
## 5. Export labels
## ============================================================

tmp_dir <- file.path(base_tmp, "predictions_tmp")
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
csv_files <- character(length(prediction_files))

for (i in seq_along(prediction_files)) {
  prediction_file <- prediction_files[[i]]
  pred_labels <- predict_labels_for_file(LDAclassifier, prediction_file, RejectionThreshold)

  csv_file <- file.path(tmp_dir, basename(prediction_file))
  data.table::fwrite(as.data.frame(pred_labels), file = csv_file, col.names = FALSE, quote = FALSE)
  csv_files[i] <- csv_file

  rm(pred_labels)
  invisible(gc(verbose = FALSE))
}

# Create tar.gz archive of all CSVs
name <- args[['name']]
tar(tarfile = glue("{output_dir}/{name}_predicted_labels.tar.gz"), files = csv_files, compression = "gzip", tar = "internal")


# Temporary workspace is cleaned by on.exit.
