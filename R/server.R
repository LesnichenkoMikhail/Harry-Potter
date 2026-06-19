server <- function(input, output, session) {

  # динамика тональности по главам с цветовой шкалой
  output$sent_plot <- renderPlotly({
    data <- sentiment_data |>
      filter(
        book == input$book_sent,
        lexicon == input$sent_lexicon
      ) |>
      arrange(chapter_num) |>
      mutate(point_color = red_gold_gradient(score_norm))

    plot_ly(
      data,
      x = ~chapter_num,
      y = ~score_norm,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#7f0909", width = 2),
      marker = list(
        color = data$point_color,
        size = 9,
        line = list(color = "#4d0707", width = 1)
      ),
      text = ~paste0(
        "Глава: ", chapter_num,
        "<br>Оценка на 1000 слов: ", round(score_norm, 2),
        "<br>Совпавших слов: ", matched_words
      ),
      hoverinfo = "text"
    ) |>
      layout(
        title = "Нормированная эмоциональная динамика",
        xaxis = list(title = "Глава", dtick = 1),
        yaxis = list(title = "Оценка тональности на 1000 слов")
      )
  })

  # частота упоминаний персонажей
  output$char_plot <- renderPlotly({
    data <- count_character_mentions(df, input$book_char) |>
      arrange(mentions) |>
      mutate(
        character = factor(character, levels = character),
        fill = grDevices::colorRampPalette(c("#d3a625", "#7f0909"))(n())
      )

    plot_ly(
      data,
      x = ~character,
      y = ~mentions,
      type = "bar",
      marker = list(color = data$fill)
    ) |>
      layout(
        title = "Частота упоминаний персонажей",
        xaxis = list(title = "Персонаж"),
        yaxis = list(title = "Количество упоминаний")
      )
  })

  # ключевые слова книги (столбцы или облако)
  output$tfidf_plot <- renderPlotly({
    data <- tfidf_data |>
      filter(book == input$book_tfidf) |>
      slice_max(tf_idf, n = if_else(input$tfidf_view == "cloud", 45, 15)) |>
      arrange(tf_idf) |>
      mutate(word = factor(word, levels = word))

    if (input$tfidf_view == "cloud") {
      # Облако слов: симуляция расположения через полярные координаты
      cloud_data <- data |>
        arrange(desc(tf_idf)) |>
        mutate(
          rank = row_number(),
          angle = rank * 2.4,
          radius = sqrt(rank),
          x = radius * cos(angle),
          y = radius * sin(angle),
          font_size = scales::rescale(tf_idf, to = c(14, 42)),
          color = grDevices::colorRampPalette(c("#1a472a", "#d3a625", "#7f0909"))(n())
        )

      plot_ly(
        cloud_data,
        x = ~x,
        y = ~y,
        text = ~as.character(word),
        type = "scatter",
        mode = "text",
        textfont = list(
          size = cloud_data$font_size,
          color = cloud_data$color
        ),
        hovertext = ~paste0(
          "Слово: ", as.character(word),
          "<br>TF-IDF: ", signif(tf_idf, 4)
        ),
        hoverinfo = "text"
      ) |>
        layout(
          title = "Облако ключевых слов",
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          showlegend = FALSE
        )
    } else {
      bar_colors <- grDevices::colorRampPalette(c("#d3a625", "#7f0909"))(nrow(data))

      plot_ly(
        data,
        x = ~tf_idf,
        y = ~word,
        type = "bar",
        orientation = "h",
        marker = list(color = bar_colors)
      ) |>
        layout(
          title = "TF-IDF: ключевые слова книги",
          xaxis = list(title = "TF-IDF"),
          yaxis = list(title = "")
        )
    }
  })

  # сеть по кнопке
  network_graph <- eventReactive(input$build_network_btn, {
    build_network(df, input$book_network)
  })

  output$network_plot <- renderVisNetwork({
    graph <- network_graph()
    vis_data <- network_vis_data(graph, input$min_connection)

    validate(
      need(nrow(vis_data$edges) > 0, "Для этого порога связей не найдено.")
    )

    # сумма силы связей для каждого узла
    node_strength <- vis_data$edges |>
      select(from, to, weight) |>
      tidyr::pivot_longer(c(from, to), values_to = "id") |>
      group_by(id) |>
      summarise(strength = sum(weight), .groups = "drop")

    nodes <- vis_data$nodes |>
      left_join(node_strength, by = "id") |>
      mutate(
        strength = replace_na(strength, 0),
        value = strength,
        color = red_gold_gradient(strength, low = "#e7c75b", high = "#7f0909"),
        title = paste("Суммарная сила связей:", strength)
      )

    edges <- vis_data$edges |>
      mutate(
        title = paste("Вес связи:", weight),
        width = pmax(1, weight / 5),
        color = red_gold_gradient(weight, low = "#e7c75b", high = "#7f0909")
      )

    visNetwork(nodes, edges) |>
      visNodes(shape = "dot", scaling = list(min = 18, max = 42)) |>
      visEdges(smooth = TRUE) |>
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
      visPhysics(stabilization = TRUE)
  })

  # классификатор по кнопкее
  prediction <- eventReactive(input$predict_btn, {
    req(str_squish(input$input_text) != "")
    predict_book(classifier_fit, input$input_text)
  })

  output$prediction_result <- renderText({
    req(prediction())
    paste("Вероятнее всего:", prediction()$book)
  })

  output$probability_table <- renderTable({
    req(prediction())

    prediction()$probabilities |>
      mutate(probability = scales::percent(probability, accuracy = 0.1)) |>
      rename(
        "Книга" = book,
        "Вероятность" = probability
      )
  })

  output$model_metrics <- renderTable({
    classifier_metrics$train_test_metrics |>
      filter(
        split == "test",
        .metric %in% c("accuracy", "bal_accuracy", "precision", "recall", "f_meas")
      ) |>
      transmute(
        "Метрика" = recode(
          .metric,
          accuracy = "Точность",
          bal_accuracy = "Сбалансированная точность",
          precision = "Макро-точность",
          recall = "Макро-полнота",
          f_meas = "Макро-F1",
          kap = "Cohen's kappa",
          mn_log_loss = "Multinomial log-loss"
        ),
        "Значение" = round(.estimate, 3)
      )
  })

  output$model_settings <- renderTable({
    classifier_metrics$settings |>
      transmute(
        "Окно чанков" = as.integer(chunks_per_doc),
        "CV folds" = as.integer(folds),
        "Макс. токенов" = as.integer(max_tokens),
        "Регуляризация lambda" = format(lambda, scientific = FALSE, trim = TRUE)
      )
  })
}
