library(readr)

source("R/classifier.R")

df <- read_csv("data/harry_potter_books.csv", show_col_types = FALSE)

artifacts <- train_classifier_model(
  df,
  chunks_per_doc = 100,
  seed = 2026,
  folds = 5,
  max_tokens = 3000,
  output_dir = "models"
)

print(artifacts$metrics$settings)
print(artifacts$metrics$regularization)
print(artifacts$metrics$train_test_metrics)
