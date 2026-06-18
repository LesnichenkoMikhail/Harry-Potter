library(dplyr)
library(glmnet)
library(hardhat)
library(parsnip)
library(readr)
library(recipes)
library(rsample)
library(stringr)
library(textrecipes)
library(tibble)
library(tidyr)
library(tune)
library(workflows)
library(yardstick)

classifier_terms <- c(
  "harry", "ron", "hermione", "dumbledore", "snape", "voldemort",
  "draco", "hagrid", "ginny", "mcgonagall", "sirius", "lupin",
  "umbridge", "slughorn", "lockhart", "quirrell", "cedric", "dobby",
  "kreacher", "bellatrix", "luna", "neville", "malfoy", "moody",
  "stone", "chamber", "basilisk", "azkaban", "dementor", "goblet",
  "tournament", "crouch", "prophecy", "horcrux", "hallows"
)

clean_classifier_text <- function(text) {
  text |>
    str_replace_all("[^A-Za-z0-9'[:space:]-]", " ") |>
    str_squish()
}

classifier_term_columns <- function() {
  paste0("term_", make.names(classifier_terms))
}

add_classifier_features <- function(data) {
  text_lower <- str_to_lower(data$text)
  tokens <- str_extract_all(text_lower, "[a-z']+")
  word_count <- pmax(lengths(tokens), 1)

  feature_data <- data |>
    mutate(
      log_words = log1p(word_count),
      unique_word_ratio = vapply(
        tokens,
        function(x) dplyr::n_distinct(x) / max(length(x), 1),
        numeric(1)
      ),
      dialogue_words_per_1000 = 1000 * str_count(
        text_lower,
        "\\b(said|asked|replied|cried|whispered|shouted)\\b"
      ) / word_count
    )

  term_features <- lapply(classifier_terms, function(term) {
    1000 * str_count(text_lower, regex(paste0("\\b", term, "\\b"))) / word_count
  }) |>
    setNames(classifier_term_columns()) |>
    as_tibble()

  bind_cols(feature_data, term_features)
}

prepare_classifier_documents <- function(df, chunks_per_doc = 100) {
  df |>
    mutate(text = clean_classifier_text(text)) |>
    group_by(book, chapter) |>
    mutate(
      chunk_in_chapter = row_number(),
      doc_in_chapter = ceiling(chunk_in_chapter / chunks_per_doc)
    ) |>
    ungroup() |>
    group_by(book, chapter, doc_in_chapter) |>
    summarise(
      text = paste(text, collapse = " "),
      chunks = n(),
      .groups = "drop"
    ) |>
    mutate(
      book = factor(book),
      chapter_num = as.integer(str_extract(chapter, "\\d+")),
      chapter_id = paste(book, chapter, sep = "::"),
      doc_key = paste(chapter_id, doc_in_chapter, sep = "::")
    ) |>
    add_classifier_features()
}

add_classifier_weights <- function(data) {
  class_counts <- data |>
    count(book, name = "class_n")
  avg_class_n <- mean(class_counts$class_n)

  data |>
    left_join(class_counts, by = "book") |>
    mutate(case_weight = importance_weights(avg_class_n / class_n)) |>
    select(-class_n)
}

split_classifier_documents <- function(docs,
                                       train_prop = 0.7,
                                       validation_prop = 0.1,
                                       seed = 2026) {
  set.seed(seed)

  chapter_index <- docs |>
    distinct(book, chapter_id) |>
    group_by(book) |>
    mutate(
      chapter_rank = sample(row_number()),
      train_chapters = pmax(1, floor(n() * train_prop)),
      validation_chapters = pmax(1, floor(n() * validation_prop)),
      split = case_when(
        chapter_rank <= train_chapters ~ "train",
        chapter_rank <= train_chapters + validation_chapters ~ "validation",
        TRUE ~ "test"
      )
    ) |>
    ungroup() |>
    select(chapter_id, split)

  docs |>
    left_join(chapter_index, by = "chapter_id") |>
    relocate(split, .after = doc_key)
}

build_classifier_recipe <- function(train_data, max_tokens = 3000, min_times = 3) {
  feature_cols <- c(
    "log_words",
    "unique_word_ratio",
    "dialogue_words_per_1000",
    classifier_term_columns()
  )

  recipe_formula <- as.formula(
    paste("book ~ text + case_weight +", paste(feature_cols, collapse = " + "))
  )

  recipe(recipe_formula, data = train_data) |>
    step_tokenize(text, token = "words") |>
    step_stopwords(text, language = "en") |>
    step_tokenfilter(text, min_times = min_times, max_tokens = max_tokens) |>
    step_tfidf(text)
}

classifier_metric_table <- function(predictions, split) {
  probability_cols <- setdiff(
    names(predictions)[startsWith(names(predictions), ".pred_")],
    ".pred_class"
  )

  bind_rows(
    accuracy(predictions, truth = book, estimate = .pred_class),
    bal_accuracy(predictions, truth = book, estimate = .pred_class),
    precision(predictions, truth = book, estimate = .pred_class, estimator = "macro"),
    recall(predictions, truth = book, estimate = .pred_class, estimator = "macro"),
    f_meas(predictions, truth = book, estimate = .pred_class, estimator = "macro"),
    kap(predictions, truth = book, estimate = .pred_class),
    mn_log_loss(predictions, truth = book, all_of(probability_cols))
  ) |>
    mutate(split = split, .before = 1)
}

train_classifier_model <- function(df,
                                   chunks_per_doc = 100,
                                   seed = 2026,
                                   folds = 5,
                                   max_tokens = 3000,
                                   penalty = 0.0001,
                                   split_output_path = "data/classifier_split_documents.csv",
                                   output_dir = "models") {
  set.seed(seed)

  docs <- prepare_classifier_documents(df, chunks_per_doc)

  split_docs <- split_classifier_documents(docs, seed = seed)

  if (!is.null(split_output_path)) {
    write_csv(
      split_docs |>
        select(split, book, chapter, chapter_num, doc_in_chapter, doc_key, chunks, text),
      split_output_path
    )
  }

  train_data <- split_docs |>
    filter(split == "train") |>
    select(-split) |>
    add_classifier_weights()

  validation_data <- split_docs |>
    filter(split == "validation") |>
    select(-split)

  final_train_data <- split_docs |>
    filter(split %in% c("train", "validation")) |>
    select(-split) |>
    add_classifier_weights()

  test_data <- split_docs |>
    filter(split == "test") |>
    select(-split)

  cv_folds <- group_vfold_cv(
    train_data,
    group = chapter_id,
    v = folds,
    strata = book
  )

  classifier_recipe <- build_classifier_recipe(
    train_data,
    max_tokens = max_tokens
  )

  classifier_model <- multinom_reg(
    penalty = penalty,
    mixture = 0.25
  ) |>
    set_engine("glmnet") |>
    set_mode("classification")

  classifier_wf <- workflow() |>
    add_recipe(classifier_recipe) |>
    add_model(classifier_model) |>
    add_case_weights(case_weight)

  cv_results <- fit_resamples(
    classifier_wf,
    resamples = cv_folds,
    metrics = metric_set(accuracy, bal_accuracy, precision, recall, f_meas, mn_log_loss),
    control = control_resamples(save_pred = FALSE)
  )

  validation_fit <- classifier_wf |>
    fit(train_data)

  final_fit <- classifier_wf |>
    fit(final_train_data)

  train_predictions <- bind_cols(
    train_data |> select(book),
    predict(validation_fit, train_data),
    predict(validation_fit, train_data, type = "prob")
  )

  validation_predictions <- bind_cols(
    validation_data |> select(book),
    predict(validation_fit, validation_data),
    predict(validation_fit, validation_data, type = "prob")
  )

  test_predictions <- bind_cols(
    test_data |> select(book),
    predict(final_fit, test_data),
    predict(final_fit, test_data, type = "prob")
  )

  metrics <- list(
    data_summary = docs |>
      count(book, name = "documents") |>
      left_join(
        df |> count(book, name = "chunks"),
        by = "book"
      ),
    split_summary = split_docs |>
      count(split, book, name = "documents") |>
      arrange(split, book),
    chapter_summary = df |>
      count(book, chapter, name = "chunks") |>
      summarise(
        chapters = n(),
        min_chunks = min(chunks),
        median_chunks = median(chunks),
        max_chunks = max(chunks),
        .by = book
      ),
    cv_metrics = collect_metrics(cv_results),
    regularization = tibble(lambda = penalty),
    train_test_metrics = bind_rows(
      classifier_metric_table(train_predictions, "train"),
      classifier_metric_table(validation_predictions, "validation"),
      classifier_metric_table(test_predictions, "test")
    ),
    confusion_matrix = conf_mat(
      test_predictions,
      truth = book,
      estimate = .pred_class
    ),
    settings = tibble(
      chunks_per_doc = chunks_per_doc,
      folds = folds,
      max_tokens = max_tokens,
      lambda = penalty,
      seed = seed
    )
  )

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(final_fit, file.path(output_dir, "classifier_fit.rds"))
    saveRDS(metrics, file.path(output_dir, "classifier_metrics.rds"))
  }

  list(model = final_fit, metrics = metrics)
}

load_classifier_artifacts <- function(model_dir = "models") {
  list(
    model = readRDS(file.path(model_dir, "classifier_fit.rds")),
    metrics = readRDS(file.path(model_dir, "classifier_metrics.rds"))
  )
}

predict_book <- function(model, text) {
  new_data <- tibble(text = clean_classifier_text(text))
  new_data <- add_classifier_features(new_data)

  predicted_book <- predict(model, new_data) |>
    pull(.pred_class) |>
    as.character()

  probabilities <- predict(model, new_data, type = "prob") |>
    pivot_longer(
      cols = everything(),
      names_to = "book",
      values_to = "probability"
    ) |>
    mutate(book = str_remove(book, "^\\.pred_")) |>
    arrange(desc(probability))

  list(book = predicted_book, probabilities = probabilities)
}
