# Загрузка библиотек
library(shiny)
library(bslib)
library(dplyr)
library(igraph)
library(plotly)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(tidytext)
library(visNetwork)

# Исходные данные
df <- read_csv("data/harry_potter_books.csv", show_col_types = FALSE)

# Подключение модулей
source("R/utils.R")
source("R/sentiment.R")
source("R/network.R")
source("R/tfidf.R")
source("R/classifier.R")

# Предрасчёт тональности, TF-IDF и загрузка модели
sentiment_data <- calculate_all_sentiments(df)
tfidf_data <- calculate_tfidf(df)
classifier_artifacts <- load_classifier_artifacts("models")
classifier_fit <- classifier_artifacts$model
classifier_metrics <- classifier_artifacts$metrics
