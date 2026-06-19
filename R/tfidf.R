# дропаем именованные сущности, забивают топ тфидф
tfidf_excluded_words <- c(
  "harry", "potter", "ron", "ronald", "hermione", "dumbledore",
  "snape", "voldemort", "draco", "hagrid", "ginny", "mcgonagall",
  "sirius", "lupin", "remus", "umbridge", "slughorn", "lockhart",
  "quirrell", "quirell", "cedric", "dobby", "kreacher", "bellatrix",
  "luna", "neville", "malfoy", "moody", "weasley", "weasleys",
  "dursley", "dursleys", "dudley", "vernon", "petunia", "fudge",
  "filch", "peeves", "firenze", "ronan", "bane", "ollivander",
  "viktor", "krum", "fleur", "tonks", "kingsley", "arthur",
  "molly", "percy", "fred", "george", "cho", "dean", "seamus",
  "lavender", "trelawney", "flitwick", "sprout", "flamel",
  "nicolas", "piers", "griphook", "flint", "norbert", "aragog",
  "aberforth", "bagman", "winky", "xenophilius", "mclaggen",
  "scrimgeour", "rita", "skeeter", "karkaroff", "maxime",
  "ludo", "pettigrew", "peter", "morfin", "gilderoy", "ogden",
  "myrtle", "greyback", "ariana", "gaunt", "marge", "cattermole",
  "bertha", "riddle", "black", "merope", "crouch", "grindelwald",
  "scabbers", "yaxley", "mom", "quot"
)

# расчёт вектора
calculate_tfidf <- function(df) {
  tfidf <- df |>
    unnest_tokens(word, text) |>
    anti_join(stop_words, by = "word") |>
    filter(
      str_detect(word, "^[a-z]+('[a-z]+)?$"),
      str_length(word) > 2,
      !str_remove(word, "'s$") %in% tfidf_excluded_words
    ) |>
    count(book, word) |>
    bind_tf_idf(word, book, n) |>
    arrange(desc(tf_idf))

  return(tfidf)
}
