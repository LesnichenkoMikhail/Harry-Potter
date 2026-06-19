# персонажи для поиска в тексте
hp_characters <- tibble(
  character = c(
    "Harry", "Ron", "Hermione", "Dumbledore", "Snape",
    "Voldemort", "Draco", "Hagrid", "Ginny", "McGonagall"
  ),
  pattern = paste0("\\b", character, "\\b")
)

# поиск персонажей в строке
find_characters <- function(text) {
  hp_characters$character[str_detect(
    text,
    regex(hp_characters$pattern, ignore_case = TRUE)
  )]
}

# частота упоминаний каждого персонажа в книге
count_character_mentions <- function(df, selected_book) {
  text <- df |>
    filter(book == selected_book) |>
    pull(text) |>
    paste(collapse = " ")

  hp_characters |>
    transmute(
      character,
      mentions = str_count(text, regex(pattern, ignore_case = TRUE))
    ) |>
    arrange(desc(mentions))
}

# граф совместных упоминаний: ребро = два персонажа в одном чанке
build_network <- function(df, selected_book) {
  data <- df |>
    filter(book == selected_book)

  edges <- lapply(data$text, function(text) {
    found <- sort(find_characters(text))

    if (length(found) < 2) {
      return(NULL)
    }

    pairs <- t(combn(found, 2))
    tibble(from = pairs[, 1], to = pairs[, 2])
  }) |>
    bind_rows() |>
    count(from, to, name = "weight")

  igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = hp_characters |> transmute(name = character)
  )
}

# фильтруем рёбра по минимальному весу и подготовка данных для visNetwork
network_vis_data <- function(graph, min_weight) {
  edges <- igraph::as_data_frame(graph, what = "edges") |>
    filter(weight >= min_weight)

  nodes <- tibble(
    id = unique(c(edges$from, edges$to)),
    label = unique(c(edges$from, edges$to))
  )

  list(nodes = nodes, edges = edges)
}
