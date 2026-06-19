# грузим лексиконы тональности
sentiment_lexicon_scores <- function(lexicon) {
  if (lexicon == "afinn") {
    readRDS("data/afinn_lexicon.rds") |>
      transmute(word, value)
  } else {
    get_sentiments("bing") |>
      transmute(
        word,
        value = if_else(sentiment == "positive", 1, -1)
      )
  }
}

# Оценка тональности с нормировкой на 1000 слов
calculate_sentiment <- function(df, lexicon = "bing") {
  tokens <- df |>
    mutate(chapter_num = as.integer(str_extract(chapter, "\\d+"))) |>
    unnest_tokens(word, text)

  totals <- tokens |>
    count(book, chapter, chapter_num, name = "total_words")

  scores <- tokens |>
    inner_join(
      sentiment_lexicon_scores(lexicon),
      by = "word",
      relationship = "many-to-many"
    ) |>
    group_by(book, chapter, chapter_num) |>
    summarise(
      score = sum(value),
      positive = sum(value > 0),
      negative = sum(value < 0),
      matched_words = n(),
      .groups = "drop"
    )

  totals |>
    left_join(scores, by = c("book", "chapter", "chapter_num")) |>
    replace_na(list(
      score = 0,
      positive = 0,
      negative = 0,
      matched_words = 0
    )) |>
    mutate(
      score_norm = 1000 * score / total_words,
      lexicon = lexicon
    ) |>
    arrange(book, chapter_num)
}

# считаем тональность по всем лексиконам
calculate_all_sentiments <- function(df, lexicons = c("bing", "afinn")) {
  bind_rows(lapply(lexicons, calculate_sentiment, df = df))
}
