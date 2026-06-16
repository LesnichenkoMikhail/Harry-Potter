library(tidytext)
library(dplyr)

calculate_tfidf <- function(df) {
  
  tfidf <- df |>
    unnest_tokens(word, text) |>
    count(book, word) |>
    bind_tf_idf(word, book, n) |>
    arrange(desc(tf_idf))
  
  return(tfidf)
}
