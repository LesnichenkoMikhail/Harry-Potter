library(dplyr)
library(tidytext)

calculate_sentiment <- function(df) {
  
  sentiment <- df |>
    unnest_tokens(word, text) |>
    inner_join(get_sentiments("bing")) |>
    count(book, chapter, sentiment) |>
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
    mutate(score = positive - negative)
  
  return(sentiment)
}
