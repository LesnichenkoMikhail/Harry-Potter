library(shiny)
library(tidyverse)
library(plotly)
library(tidytext)
library(igraph)
library(bslib)
library(tidymodels)
library(visNetwork)
library(textrecipes)

# загрузка датасета
df <- read_csv("data/harry_potter_books.csv", show_col_types = FALSE)

source("R/sentiment.R")
source("R/network.R")
source("R/tfidf.R")
source("R/classifier.R")

sentiment_data <- calculate_sentiment(df)
tfidf_data <- calculate_tfidf(df)
# network_graph <- build_network(df)


# моделька 
# моделька

# Объединяем маленькие фрагменты в более крупные тексты
df_classifier <- df |>
  group_by(book, chapter) |>
  summarise(
    text = paste(text, collapse = " "),
    .groups = "drop"
  )


# балансируем количество примеров между книгами
model_df <- df_classifier |>
  group_by(book) |>
  slice_sample(n = 200) |>
  ungroup()

set.seed(123)

split <- initial_split(model_df, prop = 0.8)
train_data <- training(split)
test_data  <- testing(split)

rec <- recipe(book ~ text, data = train_data) |>
  step_tokenize(text) |>
  step_stopwords(text) |>
  step_tokenfilter(text, max_tokens = 8000) |>
  step_tfidf(text)

model <- multinom_reg(
  penalty = 0.001
) |>
  set_engine("glmnet")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

classifier_fit <- wf |> fit(train_data)
