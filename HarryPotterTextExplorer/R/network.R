library(dplyr)
library(tidyr)
library(stringr)

build_network <- function(df, selected_book) {
  
  characters <- c(
    "Harry",
    "Ron",
    "Hermione",
    "Dumbledore",
    "Snape",
    "Voldemort",
    "Draco",
    "Hagrid",
    "Ginny",
    "McGonagall"
  )
  
  data <- df |>
    dplyr::filter(book == selected_book)
  
  edges <- data.frame(
    from = character(),
    to = character()
  )
  
  for (i in 1:nrow(data)) {
    
    text <- data$text[i]
    
    found <- characters[stringr::str_detect(
      text,
      characters
    )]
    
    if (length(found) > 1) {
      
      pairs <- t(combn(found, 2))
      
      edges <- rbind(
        edges,
        data.frame(
          from = pairs[,1],
          to = pairs[,2]
        )
      )
    }
  }
  
  
  edges <- edges |>
    dplyr::count(
      from,
      to,
      name = "weight"
    )
  
  
  graph <- igraph::graph_from_data_frame(
    edges,
    directed = FALSE
  )
  
  return(graph)
}
