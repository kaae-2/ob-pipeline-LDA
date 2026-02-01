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

# FOR TESTING
# Path to zipped data
# dataset_path <- "/Users/srz223/Documents/courses/Benchmarking/repos/ob-pipeline-cytof/out/data/data_import/dataset_name-FR-FCM-Z2KP_virus_final_seed-42/preprocessing/data_preprocessing/num-1_test-sample-limit-5"
# train_x_path <- glue("{dataset_path}/data_import.train.matrix.tar.gz")
# train_y_path <- glue("{dataset_path}/data_import.train.labels.tar.gz")
# # test_y_path <- glue("{dataset_path}/data_import.test.labels.tar.gz")
# test_x_path <- glue("{dataset_path}/data_import.test.matrices.tar.gz")

cat("Loading data...")
# ---------------------------
# LOAD TRAINING X
# ---------------------------
train_x_path <- args[['data.train_matrix']]
# train_x_files <- archive(train_x_path)$path
# 
# # Open a connection to the inner file and read it as CSV (no column names)
# train_x_list <- vector("list", length(train_x_files))
# names(train_x_list) <- train_x_files
# 
# for (file in train_x_files){
#   con <- archive_read(train_x_path, file)
#   df <- read_csv(con, col_names = FALSE)
#   train_x_list[[file]] <- df
# }

train_x_files <- utils::untar(train_x_path, list = TRUE)
train_x_list <- setNames(vector("list", length(train_x_files)), train_x_files)

# extract to a temp dir
tmp <- tempdir()
utils::untar(train_x_path, exdir = tmp)

for (file in train_x_files) {
  df <- read_csv(file.path(tmp, file), col_names = FALSE)
  train_x_list[[file]] <- df
}

# ---------------------------
# LOAD TRAINING Y
# ---------------------------
train_y_path <- args[['data.train_labels']]
# train_y_files <- archive(train_y_path)$path
# 
# # Open a connection to the inner file and read it as CSV (no column names)
# train_y_list <- vector("list", length(train_y_files))
# names(train_y_list) <- train_y_files
# 
# for (file in train_y_files){
#   con <- archive_read(train_y_path, file)
#   df <- read_csv(con, col_names = FALSE)
#   train_y_list[[file]] <- df
# }

train_y_files <- utils::untar(train_y_path, list = TRUE)
train_y_list <- setNames(vector("list", length(train_y_files)), train_y_files)

# extract to a temp dir
tmp <- tempdir()
utils::untar(train_y_path, exdir = tmp)

for (file in train_y_files) {
  df <- read_csv(file.path(tmp, file), col_names = FALSE)
  train_y_list[[file]] <- df
}

get_sample_number <- function(file_name, fallback) {
  base <- basename(file_name)
  base <- gsub("\\.csv(\\.gz)?$", "", base)
  m <- regexpr("[0-9]+(?!.*[0-9])", base, perl = TRUE)
  if (m[1] == -1) {
    return(as.character(fallback))
  }
  substr(base, m[1], m[1] + attr(m, "match.length") - 1)
}

align_training_pairs <- function(train_x_list, train_y_list) {
  x_names <- names(train_x_list)
  y_names <- names(train_y_list)
  x_ids <- vapply(seq_along(x_names), function(i) get_sample_number(x_names[i], i), character(1))
  y_ids <- vapply(seq_along(y_names), function(i) get_sample_number(y_names[i], i), character(1))

  y_id_counts <- table(y_ids)
  use_id_match <- all(x_ids %in% y_ids) && all(y_id_counts == 1)

  aligned_x <- setNames(vector("list", length(x_names)), x_names)
  aligned_y <- setNames(vector("list", length(x_names)), x_names)

  for (i in seq_along(x_names)) {
    x_name <- x_names[i]
    y_name <- if (use_id_match) {
      y_names[match(x_ids[i], y_ids)]
    } else {
      y_names[i]
    }
    if (is.na(y_name) || is.null(train_y_list[[y_name]])) {
      stop(glue("Missing labels for training sample: {x_name}"))
    }

    x <- train_x_list[[x_name]]
    y <- train_y_list[[y_name]]
    y <- y[, 1, drop = FALSE]

    valid_rows <- complete.cases(x) & apply(is.finite(as.matrix(x)), 1, all)
    if (any(!valid_rows)) {
      message("Filtering invalid rows for ", x_name, ": ", sum(!valid_rows))
      x <- x[valid_rows, , drop = FALSE]
      y <- y[valid_rows, , drop = FALSE]
    }

    if (nrow(x) != nrow(y)) {
      stop(glue(
        "Length mismatch after alignment for {x_name}: data={nrow(x)} labels={nrow(y)}"
      ))
    }

    aligned_x[[x_name]] <- x
    aligned_y[[x_name]] <- y
  }

  list(train_x = aligned_x, train_y = aligned_y)
}

aligned <- align_training_pairs(train_x_list, train_y_list)
train_x_list <- aligned$train_x
train_y_list <- aligned$train_y

# ---------------------------
# LOAD TEST X
# ---------------------------
test_x_path <- args[['data.test_matrix']]
# test_x_files <- archive(test_x_path)$path
# 
# # Open a connection to the inner file and read it as CSV (no column names)
# test_x_list <- vector("list", length(test_x_files))
# names(test_x_list) <- test_x_files
# 
# for (file in test_x_files){
#   con <- archive_read(test_x_path, file)
#   df <- read_csv(con, col_names = FALSE)
#   test_x_list[[file]] <- df
# }

test_x_files <- utils::untar(test_x_path, list = TRUE)
test_x_list <- setNames(vector("list", length(test_x_files)), test_x_files)

# extract to a temp dir
tmp <- tempdir()
utils::untar(test_x_path, exdir = tmp)

for (file in test_x_files) {
  df <- read_csv(file.path(tmp, file), col_names = FALSE)
  test_x_list[[file]] <- df
}

# ---------------------------
# RelevantMarkers needed for function 
# ---------------------------
RelevantMarkers_char <- colnames(train_x_list[[1]]) # Assuming all training samples have same columns 
names(RelevantMarkers_char) <- 1:length(RelevantMarkers_char)
RelevantMarkers <- names(RelevantMarkers_char) %>% as.integer()

# ---------------------------
# Specify paths - unique tmp folder under output dir
# ---------------------------
output_dir <- args[['output_dir']]
base_tmp <- file.path(
  output_dir,
  paste0("tmp_LDA_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%S"))
)
dir.create(base_tmp, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(base_tmp, recursive = TRUE), add = TRUE)

TrainingSamplesExt <- file.path(base_tmp, "TrainingSamplesExt")
TrainingLabelsExt <- file.path(base_tmp, "TrainingLabelsExt")
TestingSamplesExt <- file.path(base_tmp, "TestingSamplesExt")

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
  lapply(names(train_x_list), function(nm) {
    write_delim(
      x = train_x_list[[nm]],
      file = file.path(TrainingSamplesExt, glue("LDA_{nm}")), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)

invisible(
  lapply(names(train_y_list), function(nm) {
    write_delim(
      x = train_y_list[[nm]],
      file = file.path(TrainingLabelsExt, glue("LDA_{nm}")), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)

invisible(
  lapply(names(test_x_list), function(nm) {
    write_delim(
      x = test_x_list[[nm]],
      file = file.path(TestingSamplesExt, glue("LDA_{nm}")), # already have csv extension
      col_names = FALSE,
      delim = ","
    )
  })
)

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
tmp_dir <- tempdir()
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


# Delete tmp folder now that we used it
unlink("./tmp_LDA/", recursive = TRUE)
