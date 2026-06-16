server <- function(input, output, session) {
  output$sent_plot <- renderPlotly({
    
    data <- sentiment_data |>
      filter(book == input$book_sent)
    
    plot_ly(
      data,
      x = ~chapter,
      y = ~score,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      layout(
        title = "Эмоциональная динамика книги",
        xaxis = list(title = "Глава"),
        yaxis = list(title = "Sentiment score")
      )
  })
  
  output$char_plot <- renderPlotly({
    
    chars <- c(
      "Harry","Ron","Hermione","Dumbledore","Snape",
      "Voldemort","Draco","Hagrid","Ginny","McGonagall"
    )
    
    data <- df |>
      filter(book == input$book_char) |>
      mutate(text = tolower(text))
    
    counts <- sapply(chars, function(c) {
      sum(grepl(c, data$text, ignore.case = TRUE))
    })
    
    plot_ly(
      x = chars,
      y = counts,
      type = "bar"
    ) |>
      layout(
        title = "Частота появления персонажей",
        xaxis = list(title = "Персонажи"),
        yaxis = list(title = "Количество упоминаний")
      )
  })
  
  output$tfidf_plot <- renderPlotly({
    
    tfidf_data |>
      filter(book == input$book_tfidf) |>
      arrange(desc(tf_idf)) |>
      slice(1:15) |>
      plot_ly(
        x = ~reorder(word, tf_idf),
        y = ~tf_idf,
        type = "bar"
      ) |>
      layout(
        title = "TF-IDF: ключевые слова книги",
        xaxis = list(title = "Слова"),
        yaxis = list(title = "TF-IDF")
      )
  })
  
  library(visNetwork)
  
  output$network_plot <- renderVisNetwork({
    
    req(input$book_network)
    
    
    graph <- build_network(
      df,
      input$book_network
    )
    
    
    edges <- igraph::as_data_frame(
      graph,
      what = "edges"
    ) |>
      dplyr::filter(
        weight >= input$min_connection
      )
    
    
    nodes <- data.frame(
      id = unique(
        c(edges$from, edges$to)
      ),
      label = unique(
        c(edges$from, edges$to)
      )
    )
    
    
    visNetwork(
      nodes,
      edges
    ) |>
      
      visNodes(
        shape = "dot",
        size = 30
      ) |>
      
      visEdges(
        width = 2,
        smooth = TRUE
      ) |>
      
      visPhysics(
        stabilization = TRUE
      )
    
  })
  
  prediction <- eventReactive(input$predict_btn, {
    
    req(input$input_text)
    
    new_data <- data.frame(text = input$input_text)
    
    predict(classifier_fit, new_data)
  })
  
  output$prediction_result <- renderText({
    
    req(prediction())
    
    paste("Predicted book(Результат):", prediction())
  })
  
}